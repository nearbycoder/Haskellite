{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Haskellite.Platform.Linux
  ( GlobalHotkey
  , PasteTarget
  , capturePasteTarget
  , sendPasteShortcut
  , startGlobalHotkey
  , stopGlobalHotkey
  ) where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (SomeException, bracket, displayException, finally, onException, try)
import Control.Monad (void, when)
import Data.Bits ((.|.))
import Data.Char (chr, ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)
import DBus qualified
import Foreign.C.Types (CInt (..), CUInt (..), CULong (..))
import DBus
  ( ObjectPath
  , Signal (signalBody)
  , Variant
  , fromVariant
  , methodCall
  , methodCallBody
  , methodCallDestination
  , methodReturnBody
  , toVariant
  )
import DBus.Client
  ( Client
  , MatchRule (..)
  , SignalHandler
  , addMatch
  , call_
  , connectSession
  , disconnect
  , matchAny
  , removeMatch
  )
import Foreign (Ptr, alloca, peek, poke)
import Graphics.X11.Types
  ( KeyMask
  , KeyCode
  , KeySym
  , controlMask
  , grabModeAsync
  , keyPress
  , keyRelease
  , lockMask
  , mod1Mask
  , mod2Mask
  , mod4Mask
  , shiftMask
  , xK_Control_L
  , xK_v
  )
import Graphics.X11.Xlib
  ( XEventPtr
  , allocaXEvent
  , closeDisplay
  , defaultRootWindow
  , get_KeyEvent
  , get_EventType
  , grabKey
  , keysymToKeycode
  , nextEvent
  , openDisplay
  , peekEvent
  , pending
  , sync
  , ungrabKey
  )
import Graphics.X11.Xlib.Types (Display (..))
import Haskellite.Types
  ( HotkeyKey (..)
  , HotkeyModifiers (..)
  , HotkeyPreset
  , hotkeyBinding
  )
import System.Environment (lookupEnv)
import System.Timeout (timeout)

data GlobalHotkey
  = X11Hotkey (Async ())
  | PortalHotkey Client ObjectPath SignalHandler SignalHandler

data PasteTarget = PasteTarget

capturePasteTarget :: IO (Maybe PasteTarget)
capturePasteTarget = pure (Just PasteTarget)

startGlobalHotkey :: HotkeyPreset -> IO () -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey preset pressedAction releasedAction = do
  wayland <- isJust <$> lookupEnv "WAYLAND_DISPLAY"
  outcome <- try $ if wayland then startPortalHotkey preset pressedAction releasedAction else startX11Hotkey preset pressedAction releasedAction
  pure $ either (Left . exceptionText) Right outcome

startX11Hotkey :: HotkeyPreset -> IO () -> IO () -> IO GlobalHotkey
startX11Hotkey preset pressedAction releasedAction = do
    display <- openDisplay ""
    let root = defaultRootWindow display
        modifiers = hotkeyModifiers preset
    keycode <- keysymToKeycode display (hotkeyKeySym preset)
    detectableRepeat <- enableDetectableAutoRepeat display
    mapM_ (\extra -> grabKey display keycode (modifiers .|. extra) root False grabModeAsync grabModeAsync) ignoredLockMasks
    sync display False
    worker <- async $ eventLoop display keycode detectableRepeat pressedAction releasedAction `finally` release display keycode modifiers
    pure $ X11Hotkey worker

stopGlobalHotkey :: GlobalHotkey -> IO ()
stopGlobalHotkey hotkey = case hotkey of
  X11Hotkey worker -> cancel worker >> void (waitCatch worker)
  PortalHotkey client session activatedHandler deactivatedHandler -> do
    void . try @SomeException $ removeMatch client activatedHandler
    void . try @SomeException $ removeMatch client deactivatedHandler
    void . try @SomeException $
      call_
        client
        (methodCall session "org.freedesktop.portal.Session" "Close")
          { methodCallDestination = Just portalService
          }
    disconnect client

sendPasteShortcut :: Maybe PasteTarget -> IO (Either Text ())
sendPasteShortcut _ = do
  wayland <- isJust <$> lookupEnv "WAYLAND_DISPLAY"
  if wayland
    then pure $ Left "Wayland prevented synthetic paste; the dictation is on the clipboard"
    else do
      outcome <- try $ bracket (openDisplay "") closeDisplay $ \display -> do
        control <- keysymToKeycode display xK_Control_L
        letterV <- keysymToKeycode display xK_v
        _ <- xTestFakeKeyEvent display (fromIntegral control) 1 0
        _ <- xTestFakeKeyEvent display (fromIntegral letterV) 1 0
        _ <- xTestFakeKeyEvent display (fromIntegral letterV) 0 0
        _ <- xTestFakeKeyEvent display (fromIntegral control) 0 0
        sync display False
      pure $ either (Left . exceptionText) Right outcome

eventLoop :: Display -> KeyCode -> Bool -> IO () -> IO () -> IO ()
eventLoop display hotkeyCode detectableRepeat pressedAction releasedAction = allocaXEvent $ \event -> go False event
  where
    go pressed event = do
      nextEvent display event
      eventType <- get_EventType event
      case eventType of
        value | value == keyPress -> do
          (_, _, _, _, _, _, _, _, keycode, _) <- get_KeyEvent event
          when (keycode == hotkeyCode && not pressed) pressedAction
          go (pressed || keycode == hotkeyCode) event
        value | value == keyRelease -> do
          (_, _, _, _, _, _, _, _, keycode, _) <- get_KeyEvent event
          if keycode == hotkeyCode && pressed
            then do
              repeated <- if detectableRepeat then pure False else isAutoRepeatRelease display event
              if repeated
                then go True event
                else releasedAction >> go False event
            else go pressed event
        _ -> go pressed event

startPortalHotkey :: HotkeyPreset -> IO () -> IO () -> IO GlobalHotkey
startPortalHotkey preset pressedAction releasedAction = do
  client <- connectSession
  (`onException` disconnect client) do
    _ <-
      call_
        client
        (methodCall portalPath "org.freedesktop.host.portal.Registry" "Register")
          { methodCallDestination = Just portalService
          , methodCallBody =
              [ toVariant ("haskellite" :: Text)
              , toVariant (Map.empty :: Map Text Variant)
              ]
          }
    created <-
      portalRequest
        client
        "CreateSession"
        [ toVariant $
            Map.fromList
              [ ("handle_token" :: Text, toVariant ("haskellite_create" :: Text))
              , ("session_handle_token", toVariant ("haskellite_session" :: Text))
              ]
        ]
    session <- case Map.lookup "session_handle" created of
      Nothing -> ioError $ userError "The Global Shortcuts portal did not return a session"
      Just value -> parseSessionHandle value
    let shortcutProperties =
          Map.fromList
            [ ("description" :: Text, toVariant ("Start or finish Haskellite dictation" :: Text))
            , ("preferred_trigger", toVariant $ portalTrigger preset)
            ]
        shortcuts = [("dictate" :: Text, shortcutProperties)]
    _ <-
      portalRequest
        client
        "BindShortcuts"
        [ toVariant session
        , toVariant shortcuts
        , toVariant ("" :: Text)
        , toVariant $ Map.fromList [("handle_token" :: Text, toVariant ("haskellite_bind" :: Text))]
        ]
    activatedHandler <-
      addMatch
        client
        matchAny
          { matchInterface = Just portalInterface
          , matchMember = Just "Activated"
          }
        (handlePortalShortcut pressedAction)
    deactivatedHandler <-
      addMatch
        client
        matchAny
          { matchInterface = Just portalInterface
          , matchMember = Just "Deactivated"
          }
        (handlePortalShortcut releasedAction)
    pure $ PortalHotkey client session activatedHandler deactivatedHandler

portalRequest :: Client -> Text -> [Variant] -> IO (Map Text Variant)
portalRequest client member body = do
  response <- newEmptyMVar
  handler <-
    addMatch
      client
      matchAny
        { matchInterface = Just "org.freedesktop.portal.Request"
        , matchMember = Just "Response"
        }
      (capturePortalResponse response)
  let request =
        call_
          client
          (methodCall portalPath portalInterface (fromString $ Text.unpack member))
            { methodCallDestination = Just portalService
            , methodCallBody = body
            }
  result <-
    ( do
        reply <- request
        when (null $ methodReturnBody reply) $ ioError (userError "The portal returned no request handle")
        timeout (120 * 1000000) (takeMVar response) >>= maybe (ioError $ userError "The portal request timed out") pure
    ) `finally` removeMatch client handler
  case result of
    (0, values) -> pure values
    (code, _) -> ioError . userError $ "The global shortcut request was declined (response " <> show code <> ")"

capturePortalResponse :: MVar (Word32, Map Text Variant) -> Signal -> IO ()
capturePortalResponse response signal =
  case signalBody signal of
    [statusValue, resultValue]
      | Just status <- fromVariant statusValue
      , Just results <- fromVariant resultValue -> void $ tryPutMVar response (status, results)
    _ -> pure ()

parseSessionHandle :: Variant -> IO ObjectPath
parseSessionHandle value =
  case fromVariant value of
    Just path -> pure path
    Nothing -> case fromVariant value of
      Just textValue -> pure . fromString $ Text.unpack (textValue :: Text)
      Nothing -> ioError $ userError "The Global Shortcuts portal returned an invalid session handle"

handlePortalShortcut :: IO () -> Signal -> IO ()
handlePortalShortcut action signal =
  case signalBody signal of
    (_ : shortcutValue : _)
      | Just shortcutId <- fromVariant shortcutValue
      , shortcutId == ("dictate" :: Text) -> action
    _ -> pure ()

portalTrigger :: HotkeyPreset -> Text
portalTrigger preset = Text.intercalate "+" $ modifierTokens <> [portalKeyToken key]
  where
    (modifiers, key) = hotkeyBinding preset
    modifierTokens =
      concat
        [ ["CTRL" | modifierControl modifiers]
        , ["SHIFT" | modifierShift modifiers]
        , ["ALT" | modifierAlt modifiers]
        , ["LOGO" | modifierSuper modifiers]
        ]

portalService :: DBus.BusName
portalService = "org.freedesktop.portal.Desktop"

portalPath :: ObjectPath
portalPath = "/org/freedesktop/portal/desktop"

portalInterface :: DBus.InterfaceName
portalInterface = "org.freedesktop.portal.GlobalShortcuts"

release :: Display -> KeyCode -> KeyMask -> IO ()
release display keycode modifiers = do
  let root = defaultRootWindow display
  mapM_ (\extra -> ungrabKey display keycode (modifiers .|. extra) root) ignoredLockMasks
  sync display False
  closeDisplay display

ignoredLockMasks :: [KeyMask]
ignoredLockMasks = [0, lockMask, mod2Mask, lockMask .|. mod2Mask]

hotkeyModifiers :: HotkeyPreset -> KeyMask
hotkeyModifiers preset =
  foldr (.|.) 0 $
    concat
      [ [controlMask | modifierControl modifiers]
      , [shiftMask | modifierShift modifiers]
      , [mod1Mask | modifierAlt modifiers]
      , [mod4Mask | modifierSuper modifiers]
      ]
  where
    (modifiers, _) = hotkeyBinding preset

hotkeyKeySym :: HotkeyPreset -> KeySym
hotkeyKeySym preset = keySym key
  where
    (_, key) = hotkeyBinding preset

keySym :: HotkeyKey -> KeySym
keySym key
  | key >= HotkeyA && key <= HotkeyZ = fromIntegral $ ord 'a' + fromEnum key - fromEnum HotkeyA
  | key >= Hotkey0 && key <= Hotkey9 = fromIntegral $ ord '0' + fromEnum key - fromEnum Hotkey0
  | key >= HotkeyF1 && key <= HotkeyF12 = fromIntegral $ 0xFFBE + fromEnum key - fromEnum HotkeyF1
  | otherwise = case key of
      HotkeySpace -> 0x0020
      HotkeyTab -> 0xFF09
      HotkeyReturn -> 0xFF0D
      HotkeyEscape -> 0xFF1B
      HotkeyBackspace -> 0xFF08
      HotkeyLeft -> 0xFF51
      HotkeyUp -> 0xFF52
      HotkeyRight -> 0xFF53
      HotkeyDown -> 0xFF54
      HotkeyHome -> 0xFF50
      HotkeyEnd -> 0xFF57
      HotkeyPageUp -> 0xFF55
      HotkeyPageDown -> 0xFF56
      HotkeyInsert -> 0xFF63
      HotkeyDelete -> 0xFFFF
      HotkeyMinus -> 0x002D
      HotkeyEquals -> 0x003D
      HotkeyLeftBracket -> 0x005B
      HotkeyRightBracket -> 0x005D
      HotkeyBackslash -> 0x005C
      HotkeySemicolon -> 0x003B
      HotkeyQuote -> 0x0027
      HotkeyBackquote -> 0x0060
      HotkeyComma -> 0x002C
      HotkeyPeriod -> 0x002E
      HotkeySlash -> 0x002F
      _ -> 0

portalKeyToken :: HotkeyKey -> Text
portalKeyToken key
  | key >= HotkeyA && key <= HotkeyZ = Text.singleton . chr $ ord 'a' + fromEnum key - fromEnum HotkeyA
  | key >= Hotkey0 && key <= Hotkey9 = Text.singleton . chr $ ord '0' + fromEnum key - fromEnum Hotkey0
  | key >= HotkeyF1 && key <= HotkeyF12 = "F" <> Text.pack (show $ fromEnum key - fromEnum HotkeyF1 + 1)
  | otherwise = case key of
      HotkeySpace -> "space"
      HotkeyTab -> "Tab"
      HotkeyReturn -> "Return"
      HotkeyEscape -> "Escape"
      HotkeyBackspace -> "BackSpace"
      HotkeyLeft -> "Left"
      HotkeyRight -> "Right"
      HotkeyUp -> "Up"
      HotkeyDown -> "Down"
      HotkeyHome -> "Home"
      HotkeyEnd -> "End"
      HotkeyPageUp -> "Page_Up"
      HotkeyPageDown -> "Page_Down"
      HotkeyInsert -> "Insert"
      HotkeyDelete -> "Delete"
      HotkeyMinus -> "minus"
      HotkeyEquals -> "equal"
      HotkeyLeftBracket -> "bracketleft"
      HotkeyRightBracket -> "bracketright"
      HotkeyBackslash -> "backslash"
      HotkeySemicolon -> "semicolon"
      HotkeyQuote -> "apostrophe"
      HotkeyBackquote -> "grave"
      HotkeyComma -> "comma"
      HotkeyPeriod -> "period"
      HotkeySlash -> "slash"
      _ -> "space"

enableDetectableAutoRepeat :: Display -> IO Bool
enableDetectableAutoRepeat display =
  alloca $ \supportedPointer -> do
    poke supportedPointer 0
    enabled <- xkbSetDetectableAutoRepeat display 1 supportedPointer
    supported <- peek supportedPointer
    pure $ enabled /= 0 && supported /= 0

isAutoRepeatRelease :: Display -> XEventPtr -> IO Bool
isAutoRepeatRelease display releasedEvent = do
  (_, _, releasedAt, _, _, _, _, _, releasedKey, _) <- get_KeyEvent releasedEvent
  queued <- pending display
  if queued <= 0
    then pure False
    else allocaXEvent $ \next -> do
      peekEvent display next
      nextType <- get_EventType next
      (_, _, pressedAt, _, _, _, _, _, pressedKey, _) <- get_KeyEvent next
      pure $ nextType == keyPress && pressedKey == releasedKey && pressedAt == releasedAt

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

foreign import ccall unsafe "XTestFakeKeyEvent"
  xTestFakeKeyEvent :: Display -> CUInt -> CInt -> CULong -> IO CInt

foreign import ccall unsafe "XkbSetDetectableAutoRepeat"
  xkbSetDetectableAutoRepeat :: Display -> CInt -> Ptr CInt -> IO CInt
