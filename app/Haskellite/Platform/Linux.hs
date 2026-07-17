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
  , xK_F8
  , xK_F9
  , xK_space
  , xK_v
  )
import Graphics.X11.Xlib
  ( allocaXEvent
  , closeDisplay
  , defaultRootWindow
  , get_EventType
  , grabKey
  , keysymToKeycode
  , nextEvent
  , openDisplay
  , sync
  , ungrabKey
  )
import Graphics.X11.Xlib.Types (Display (..))
import Haskellite.Types (HotkeyPreset (..))
import System.Environment (lookupEnv)
import System.Timeout (timeout)

data GlobalHotkey
  = X11Hotkey (Async ())
  | PortalHotkey Client ObjectPath SignalHandler

data PasteTarget = PasteTarget

capturePasteTarget :: IO (Maybe PasteTarget)
capturePasteTarget = pure (Just PasteTarget)

startGlobalHotkey :: HotkeyPreset -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey preset action = do
  wayland <- isJust <$> lookupEnv "WAYLAND_DISPLAY"
  outcome <- try $ if wayland then startPortalHotkey preset action else startX11Hotkey preset action
  pure $ either (Left . exceptionText) Right outcome

startX11Hotkey :: HotkeyPreset -> IO () -> IO GlobalHotkey
startX11Hotkey preset action = do
    display <- openDisplay ""
    let root = defaultRootWindow display
        modifiers = hotkeyModifiers preset
    keycode <- keysymToKeycode display (hotkeyKeySym preset)
    mapM_ (\extra -> grabKey display keycode (modifiers .|. extra) root False grabModeAsync grabModeAsync) ignoredLockMasks
    sync display False
    worker <- async $ eventLoop display action `finally` release display keycode modifiers
    pure $ X11Hotkey worker

stopGlobalHotkey :: GlobalHotkey -> IO ()
stopGlobalHotkey hotkey = case hotkey of
  X11Hotkey worker -> cancel worker >> void (waitCatch worker)
  PortalHotkey client session handler -> do
    void . try @SomeException $ removeMatch client handler
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

eventLoop :: Display -> IO () -> IO ()
eventLoop display action = allocaXEvent $ \event -> go False event
  where
    go pressed event = do
      nextEvent display event
      eventType <- get_EventType event
      case eventType of
        value | value == keyPress -> do
          when (not pressed) action
          go True event
        value | value == keyRelease -> go False event
        _ -> go pressed event

startPortalHotkey :: HotkeyPreset -> IO () -> IO GlobalHotkey
startPortalHotkey preset action = do
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
    handler <-
      addMatch
        client
        matchAny
          { matchInterface = Just portalInterface
          , matchMember = Just "Activated"
          }
        (handlePortalActivation action)
    pure $ PortalHotkey client session handler

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

handlePortalActivation :: IO () -> Signal -> IO ()
handlePortalActivation action signal =
  case signalBody signal of
    (_ : shortcutValue : _)
      | Just shortcutId <- fromVariant shortcutValue
      , shortcutId == ("dictate" :: Text) -> action
    _ -> pure ()

portalTrigger :: HotkeyPreset -> Text
portalTrigger preset = case preset of
  ControlShiftSpace -> "CTRL+SHIFT+space"
  ControlAltSpace -> "CTRL+ALT+space"
  SuperShiftSpace -> "LOGO+SHIFT+space"
  FunctionKey8 -> "F8"
  FunctionKey9 -> "F9"

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
hotkeyModifiers preset = case preset of
  ControlShiftSpace -> controlMask .|. shiftMask
  ControlAltSpace -> controlMask .|. mod1Mask
  SuperShiftSpace -> mod4Mask .|. shiftMask
  FunctionKey8 -> 0
  FunctionKey9 -> 0

hotkeyKeySym :: HotkeyPreset -> KeySym
hotkeyKeySym preset = case preset of
  ControlShiftSpace -> xK_space
  ControlAltSpace -> xK_space
  SuperShiftSpace -> xK_space
  FunctionKey8 -> xK_F8
  FunctionKey9 -> xK_F9

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

foreign import ccall unsafe "XTestFakeKeyEvent"
  xTestFakeKeyEvent :: Display -> CUInt -> CInt -> CULong -> IO CInt
