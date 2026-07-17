{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Platform.Mac
  ( GlobalHotkey
  , sendPasteShortcut
  , startGlobalHotkey
  , stopGlobalHotkey
  ) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, asyncBound, cancel, waitCatch)
import Control.Exception (SomeException, bracket, displayException, finally, try)
import Control.Monad (void, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word32, Word64)
import Foreign (FunPtr, Ptr, freeHaskellFunPtr, nullPtr, peek)
import Foreign.C.Types (CBool (..), CLong (..))
import Haskellite.Types (HotkeyPreset (..))

data GlobalHotkey = GlobalHotkey
  { workerThread :: Async ()
  , workerRunLoop :: Ptr ()
  , eventCallback :: FunPtr EventTapCallback
  }

type EventTapCallback = Ptr () -> Word32 -> Ptr () -> Ptr () -> IO (Ptr ())

startGlobalHotkey :: HotkeyPreset -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey preset action = do
  ready <- newEmptyMVar
  callback <- makeEventTapCallback $ \_ eventType event _ -> do
    when (eventType == keyDownEvent) do
      keyCode <- fromIntegral <$> cgEventGetIntegerValueField event keyboardKeycodeField
      autoRepeat <- cgEventGetIntegerValueField event keyboardAutorepeatField
      flags <- cgEventGetFlags event
      when (autoRepeat == 0 && matchesHotkey preset keyCode flags) action
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

sendPasteShortcut :: IO (Either Text ())
sendPasteShortcut = do
  outcome <- try $
    bracket (cgEventCreateKeyboardEvent nullPtr virtualV 1) cfRelease $ \down ->
      bracket (cgEventCreateKeyboardEvent nullPtr virtualV 0) cfRelease $ \up -> do
        when (down == nullPtr || up == nullPtr) $ ioError (userError "macOS could not create a keyboard event")
        cgEventSetFlags down commandMask
        cgEventSetFlags up commandMask
        cgEventPost hidEventTap down
        cgEventPost hidEventTap up
  pure $ either (Left . exceptionText) Right outcome

runEventTap :: FunPtr EventTapCallback -> MVar (Either Text (Ptr ())) -> IO ()
runEventTap callback ready = do
  tap <- cgEventTapCreate sessionEventTap headInsertEventTap listenOnlyEventTap (1 `shiftL` fromIntegral keyDownEvent) callback nullPtr
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
    (expectedKey, expectedModifiers) = case preset of
      ControlShiftSpace -> (virtualSpace, controlMask .|. shiftMask)
      ControlAltSpace -> (virtualSpace, controlMask .|. optionMask)
      SuperShiftSpace -> (virtualSpace, commandMask .|. shiftMask)
      FunctionKey8 -> (virtualF8, 0)
      FunctionKey9 -> (virtualF9, 0)

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

sessionEventTap, headInsertEventTap, listenOnlyEventTap, hidEventTap, keyDownEvent :: Word32
sessionEventTap = 1
headInsertEventTap = 0
listenOnlyEventTap = 1
hidEventTap = 0
keyDownEvent = 10

keyboardAutorepeatField, keyboardKeycodeField :: Word32
keyboardAutorepeatField = 8
keyboardKeycodeField = 9

shiftMask, controlMask, optionMask, commandMask, modifierMask :: Word64
shiftMask = 0x00020000
controlMask = 0x00040000
optionMask = 0x00080000
commandMask = 0x00100000
modifierMask = shiftMask .|. controlMask .|. optionMask .|. commandMask

virtualV, virtualSpace, virtualF8, virtualF9 :: Word16
virtualV = 9
virtualSpace = 49
virtualF8 = 100
virtualF9 = 101

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

foreign import ccall unsafe "CGEventSetFlags"
  cgEventSetFlags :: Ptr () -> Word64 -> IO ()

foreign import ccall unsafe "CGEventPost"
  cgEventPost :: Word32 -> Ptr () -> IO ()

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
