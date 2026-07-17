{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Platform.Mac
  ( GlobalHotkey
  , PasteTarget
  , capturePasteTarget
  , sendPasteShortcut
  , startGlobalHotkey
  , stopGlobalHotkey
  ) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (Async, asyncBound, cancel, waitCatch)
import Control.Exception (SomeException, bracket, displayException, finally, try)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word32, Word64)
import Foreign (FunPtr, Ptr, freeHaskellFunPtr, nullPtr, peek)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..), CInt (..), CLong (..), CULong (..))
import Haskellite.Types
  ( HotkeyKey (..)
  , HotkeyModifiers (..)
  , HotkeyPreset
  , hotkeyBinding
  )

data GlobalHotkey = GlobalHotkey
  { workerThread :: Async ()
  , workerRunLoop :: Ptr ()
  , eventCallback :: FunPtr EventTapCallback
  }

newtype PasteTarget = PasteTarget CInt

type EventTapCallback = Ptr () -> Word32 -> Ptr () -> Ptr () -> IO (Ptr ())

startGlobalHotkey :: HotkeyPreset -> IO () -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey preset pressedAction releasedAction = do
  ready <- newEmptyMVar
  pressedRef <- newIORef False
  callback <- makeEventTapCallback $ \_ eventType event _ -> do
    when (eventType == keyDownEvent) do
      keyCode <- fromIntegral <$> cgEventGetIntegerValueField event keyboardKeycodeField
      autoRepeat <- cgEventGetIntegerValueField event keyboardAutorepeatField
      flags <- cgEventGetFlags event
      alreadyPressed <- readIORef pressedRef
      when (autoRepeat == 0 && not alreadyPressed && matchesHotkey preset keyCode flags) do
        writeIORef pressedRef True
        pressedAction
    when (eventType == keyUpEvent) do
      keyCode <- fromIntegral <$> cgEventGetIntegerValueField event keyboardKeycodeField
      alreadyPressed <- readIORef pressedRef
      when (alreadyPressed && keyCode == hotkeyKeyCode preset) do
        writeIORef pressedRef False
        releasedAction
    pure event
  worker <- asyncBound $ runEventTap callback ready
  takeMVar ready >>= \case
    Left message -> do
      cancel worker
      void $ waitCatch worker
      freeHaskellFunPtr callback
      pure $ Left message
    Right runLoop -> pure . Right $ GlobalHotkey worker runLoop callback

stopGlobalHotkey :: GlobalHotkey -> IO ()
stopGlobalHotkey hotkey = do
  cfRunLoopStop (workerRunLoop hotkey)
  void $ waitCatch (workerThread hotkey)
  freeHaskellFunPtr (eventCallback hotkey)

capturePasteTarget :: IO (Maybe PasteTarget)
capturePasteTarget = do
  outcome <- try $ do
    workspaceClass <- objcClass "NSWorkspace"
    workspace <- msgSendPointer workspaceClass =<< selector "sharedWorkspace"
    frontmost <- msgSendPointer workspace =<< selector "frontmostApplication"
    runningClass <- objcClass "NSRunningApplication"
    current <- msgSendPointer runningClass =<< selector "currentApplication"
    if frontmost == nullPtr || current == nullPtr
      then pure Nothing
      else do
        frontmostPid <- msgSendCIntResult frontmost =<< selector "processIdentifier"
        currentPid <- msgSendCIntResult current =<< selector "processIdentifier"
        pure $
          if frontmostPid > 0 && frontmostPid /= currentPid
            then Just (PasteTarget frontmostPid)
            else Nothing
  pure $ either (const Nothing) id (outcome :: Either SomeException (Maybe PasteTarget))

sendPasteShortcut :: Maybe PasteTarget -> IO (Either Text ())
sendPasteShortcut Nothing = pure $ Left "macOS could not remember the application that owned the focused field"
sendPasteShortcut (Just target) = do
  outcome <- try $ do
    ensurePostEventAccess
    restorePasteTarget target
    postPasteChord
  pure $ either (Left . exceptionText) Right outcome

ensurePostEventAccess :: IO ()
ensurePostEventAccess = do
  trusted <- cgPreflightPostEventAccess
  unless (trusted /= 0) do
    granted <- cgRequestPostEventAccess
    unless (granted /= 0) $
      ioError . userError $
        "Allow Haskellite in System Settings > Privacy & Security > Accessibility, then try dictation again"

postPasteChord :: IO ()
postPasteChord = withCFObject "keyboard event source" (cgEventSourceCreate combinedSessionState) $ \source -> do
  postKey source virtualCommand True commandMask
  threadDelay 8000
  postKey source virtualV True commandMask
  threadDelay 8000
  postKey source virtualV False commandMask
  threadDelay 8000
  postKey source virtualCommand False 0

postKey :: Ptr () -> Word16 -> Bool -> Word64 -> IO ()
postKey source keyCode pressed flags =
  withCFObject "keyboard event" (cgEventCreateKeyboardEvent source keyCode $ if pressed then 1 else 0) $ \event -> do
    cgEventSetFlags event flags
    cgEventPost hidEventTap event

withCFObject :: String -> IO (Ptr ()) -> (Ptr () -> IO a) -> IO a
withCFObject description create action = do
  pointer <- create
  when (pointer == nullPtr) . ioError . userError $ "macOS could not create a " <> description
  bracket (pure pointer) cfRelease action

restorePasteTarget :: PasteTarget -> IO ()
restorePasteTarget (PasteTarget processId) = do
  runningClass <- objcClass "NSRunningApplication"
  applicationSelector <- selector "runningApplicationWithProcessIdentifier:"
  target <- msgSendCIntArgument runningClass applicationSelector processId
  when (target == nullPtr) $ ioError (userError "The application that owned the focused field is no longer running")
  active <- msgSendBool target =<< selector "isActive"
  unless (active /= 0) do
    activateSelector <- selector "activateWithOptions:"
    activated <- msgSendCULongArgument target activateSelector activateIgnoringOtherApps
    unless (activated /= 0) $ ioError (userError "macOS did not restore the application that owned the focused field")
    waitUntilActive target 12
    threadDelay 50000

waitUntilActive :: Ptr () -> Int -> IO ()
waitUntilActive _ 0 = ioError (userError "Timed out while restoring the application that owned the focused field")
waitUntilActive application attempts = do
  active <- msgSendBool application =<< selector "isActive"
  unless (active /= 0) $ threadDelay 50000 >> waitUntilActive application (attempts - 1)

activateIgnoringOtherApps :: CULong
activateIgnoringOtherApps = 2

objcClass :: String -> IO (Ptr ())
objcClass name = withCString name $ \pointer -> do
  value <- objcGetClass pointer
  when (value == nullPtr) $ ioError . userError $ "Missing macOS class: " <> name
  pure value

selector :: String -> IO (Ptr ())
selector name = withCString name selRegisterName

runEventTap :: FunPtr EventTapCallback -> MVar (Either Text (Ptr ())) -> IO ()
runEventTap callback ready = do
  let eventMask =
        (1 `shiftL` fromIntegral keyDownEvent)
          .|. (1 `shiftL` fromIntegral keyUpEvent)
  tap <- cgEventTapCreate sessionEventTap headInsertEventTap listenOnlyEventTap eventMask callback nullPtr
  if tap == nullPtr
    then putMVar ready (Left "macOS did not grant Input Monitoring permission for the global shortcut")
    else do
      source <- cfMachPortCreateRunLoopSource nullPtr tap 0
      if source == nullPtr
        then cfRelease tap >> putMVar ready (Left "macOS could not create the global shortcut event source")
        else do
          runLoop <- cfRunLoopGetCurrent
          mode <- peek cfRunLoopCommonModes
          cfRunLoopAddSource runLoop source mode
          cgEventTapEnable tap 1
          putMVar ready (Right runLoop)
          cfRunLoopRun `finally` (cfRelease source >> cfRelease tap)

matchesHotkey :: HotkeyPreset -> Word16 -> Word64 -> Bool
matchesHotkey preset keyCode flags =
  keyCode == expectedKey && flags .&. modifierMask == expectedModifiers
  where
    expectedKey = hotkeyKeyCode preset
    (modifiers, _) = hotkeyBinding preset
    expectedModifiers = macModifierMask modifiers

hotkeyKeyCode :: HotkeyPreset -> Word16
hotkeyKeyCode = macKeyCode . snd . hotkeyBinding

macModifierMask :: HotkeyModifiers -> Word64
macModifierMask modifiers =
  foldr
    (.|.)
    0
    $ concat
      [ [controlMask | modifierControl modifiers]
      , [shiftMask | modifierShift modifiers]
      , [optionMask | modifierAlt modifiers]
      , [commandMask | modifierSuper modifiers]
      ]

macKeyCode :: HotkeyKey -> Word16
macKeyCode key
  | key >= HotkeyA && key <= HotkeyZ = macLetterKeyCodes !! (fromEnum key - fromEnum HotkeyA)
  | key >= Hotkey0 && key <= Hotkey9 = macDigitKeyCodes !! (fromEnum key - fromEnum Hotkey0)
  | key >= HotkeyF1 && key <= HotkeyF12 = macFunctionKeyCodes !! (fromEnum key - fromEnum HotkeyF1)
  | otherwise = case key of
      HotkeySpace -> 49
      HotkeyTab -> 48
      HotkeyReturn -> 36
      HotkeyEscape -> 53
      HotkeyBackspace -> 51
      HotkeyLeft -> 123
      HotkeyRight -> 124
      HotkeyUp -> 126
      HotkeyDown -> 125
      HotkeyHome -> 115
      HotkeyEnd -> 119
      HotkeyPageUp -> 116
      HotkeyPageDown -> 121
      HotkeyInsert -> 114
      HotkeyDelete -> 117
      HotkeyMinus -> 27
      HotkeyEquals -> 24
      HotkeyLeftBracket -> 33
      HotkeyRightBracket -> 30
      HotkeyBackslash -> 42
      HotkeySemicolon -> 41
      HotkeyQuote -> 39
      HotkeyBackquote -> 50
      HotkeyComma -> 43
      HotkeyPeriod -> 47
      HotkeySlash -> 44
      _ -> 0

macLetterKeyCodes :: [Word16]
macLetterKeyCodes =
  [ 0
  , 11
  , 8
  , 2
  , 14
  , 3
  , 5
  , 4
  , 34
  , 38
  , 40
  , 37
  , 46
  , 45
  , 31
  , 35
  , 12
  , 15
  , 1
  , 17
  , 32
  , 9
  , 13
  , 7
  , 16
  , 6
  ]

macDigitKeyCodes :: [Word16]
macDigitKeyCodes = [29, 18, 19, 20, 21, 23, 22, 26, 28, 25]

macFunctionKeyCodes :: [Word16]
macFunctionKeyCodes = [122, 120, 99, 118, 96, 97, 98, 100, 101, 109, 103, 111]

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

sessionEventTap, headInsertEventTap, listenOnlyEventTap, hidEventTap, keyDownEvent, keyUpEvent :: Word32
sessionEventTap = 1
headInsertEventTap = 0
listenOnlyEventTap = 1
hidEventTap = 0
keyDownEvent = 10
keyUpEvent = 11

keyboardAutorepeatField, keyboardKeycodeField :: Word32
keyboardAutorepeatField = 8
keyboardKeycodeField = 9

shiftMask, controlMask, optionMask, commandMask, modifierMask :: Word64
shiftMask = 0x00020000
controlMask = 0x00040000
optionMask = 0x00080000
commandMask = 0x00100000
modifierMask = shiftMask .|. controlMask .|. optionMask .|. commandMask

virtualV :: Word16
virtualV = 9

virtualCommand :: Word16
virtualCommand = 55

combinedSessionState :: CInt
combinedSessionState = 0

foreign import ccall "wrapper"
  makeEventTapCallback :: EventTapCallback -> IO (FunPtr EventTapCallback)

foreign import ccall unsafe "CGEventTapCreate"
  cgEventTapCreate :: Word32 -> Word32 -> Word32 -> Word64 -> FunPtr EventTapCallback -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "CGEventTapEnable"
  cgEventTapEnable :: Ptr () -> CBool -> IO ()

foreign import ccall unsafe "CGEventGetIntegerValueField"
  cgEventGetIntegerValueField :: Ptr () -> Word32 -> IO CLong

foreign import ccall unsafe "CGEventGetFlags"
  cgEventGetFlags :: Ptr () -> IO Word64

foreign import ccall unsafe "CGEventCreateKeyboardEvent"
  cgEventCreateKeyboardEvent :: Ptr () -> Word16 -> CBool -> IO (Ptr ())

foreign import ccall unsafe "CGEventSourceCreate"
  cgEventSourceCreate :: CInt -> IO (Ptr ())

foreign import ccall unsafe "CGEventSetFlags"
  cgEventSetFlags :: Ptr () -> Word64 -> IO ()

foreign import ccall unsafe "CGEventPost"
  cgEventPost :: Word32 -> Ptr () -> IO ()

foreign import ccall unsafe "CGPreflightPostEventAccess"
  cgPreflightPostEventAccess :: IO CBool

foreign import ccall unsafe "CGRequestPostEventAccess"
  cgRequestPostEventAccess :: IO CBool

foreign import ccall unsafe "CFMachPortCreateRunLoopSource"
  cfMachPortCreateRunLoopSource :: Ptr () -> Ptr () -> CLong -> IO (Ptr ())

foreign import ccall unsafe "CFRunLoopGetCurrent"
  cfRunLoopGetCurrent :: IO (Ptr ())

foreign import ccall unsafe "CFRunLoopAddSource"
  cfRunLoopAddSource :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall safe "CFRunLoopRun"
  cfRunLoopRun :: IO ()

foreign import ccall unsafe "CFRunLoopStop"
  cfRunLoopStop :: Ptr () -> IO ()

foreign import ccall unsafe "CFRelease"
  cfRelease :: Ptr () -> IO ()

foreign import ccall unsafe "&kCFRunLoopCommonModes"
  cfRunLoopCommonModes :: Ptr (Ptr ())

foreign import ccall unsafe "objc_getClass"
  objcGetClass :: CString -> IO (Ptr ())

foreign import ccall unsafe "sel_registerName"
  selRegisterName :: CString -> IO (Ptr ())

foreign import ccall unsafe "objc_msgSend"
  msgSendPointer :: Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "objc_msgSend"
  msgSendCIntResult :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "objc_msgSend"
  msgSendCIntArgument :: Ptr () -> Ptr () -> CInt -> IO (Ptr ())

foreign import ccall unsafe "objc_msgSend"
  msgSendCULongArgument :: Ptr () -> Ptr () -> CULong -> IO CBool

foreign import ccall unsafe "objc_msgSend"
  msgSendBool :: Ptr () -> Ptr () -> IO CBool
