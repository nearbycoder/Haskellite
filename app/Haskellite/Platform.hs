{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Platform
  ( GlobalHotkey
  , PasteTarget
  , SystemTray
  , capturePasteTarget
  , hotkeyLabel
  , sendPasteShortcut
  , startGlobalHotkey
  , startSystemTray
  , stopGlobalHotkey
  , stopSystemTray
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Haskellite.Types
  ( HotkeyKey (..)
  , HotkeyModifiers (..)
  , HotkeyPreset
  , hotkeyBinding
  )
import qualified SDL
#if defined(mingw32_HOST_OS)
import Haskellite.Platform.Windows qualified as Native
import Haskellite.Tray.Windows qualified as Tray
#elif defined(darwin_HOST_OS)
import Haskellite.Platform.Mac qualified as Native
import Haskellite.Tray.Mac qualified as Tray
#else
import Haskellite.Platform.Linux qualified as Native
import Haskellite.Tray.Linux qualified as Tray
#endif

type GlobalHotkey = Native.GlobalHotkey
type PasteTarget = Native.PasteTarget
type SystemTray = Tray.SystemTray

capturePasteTarget :: IO (Maybe PasteTarget)
capturePasteTarget = Native.capturePasteTarget

startGlobalHotkey :: HotkeyPreset -> IO () -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey = Native.startGlobalHotkey

stopGlobalHotkey :: GlobalHotkey -> IO ()
stopGlobalHotkey = Native.stopGlobalHotkey

sendPasteShortcut :: Maybe PasteTarget -> IO (Either Text ())
sendPasteShortcut = Native.sendPasteShortcut

startSystemTray :: SDL.Window -> IO () -> IO () -> IO (Either Text SystemTray)
startSystemTray = Tray.startSystemTray

stopSystemTray :: SystemTray -> IO ()
stopSystemTray = Tray.stopSystemTray

hotkeyLabel :: HotkeyPreset -> Text
hotkeyLabel preset = Text.intercalate " + " $ modifierLabels <> [hotkeyKeyLabel key]
  where
    (modifiers, key) = hotkeyBinding preset
    modifierLabels =
      concat
        [ [controlLabel | modifierControl modifiers]
        , ["Shift" | modifierShift modifiers]
        , [altLabel | modifierAlt modifiers]
        , [superLabel | modifierSuper modifiers]
        ]

controlLabel, altLabel, superLabel :: Text
#if defined(darwin_HOST_OS)
controlLabel = "Control"
altLabel = "Option"
superLabel = "Command"
#else
controlLabel = "Ctrl"
altLabel = "Alt"
superLabel = "Super"
#endif

hotkeyKeyLabel :: HotkeyKey -> Text
hotkeyKeyLabel key = case key of
  HotkeyMinus -> "-"
  HotkeyEquals -> "="
  HotkeyLeftBracket -> "["
  HotkeyRightBracket -> "]"
  HotkeyBackslash -> "\\"
  HotkeySemicolon -> ";"
  HotkeyQuote -> "'"
  HotkeyBackquote -> "`"
  HotkeyComma -> ","
  HotkeyPeriod -> "."
  HotkeySlash -> "/"
  _ -> Text.drop 6 . Text.pack $ show key
