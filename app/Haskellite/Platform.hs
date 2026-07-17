{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Platform
  ( GlobalHotkey
  , SystemTray
  , hotkeyLabel
  , sendPasteShortcut
  , startGlobalHotkey
  , startSystemTray
  , stopGlobalHotkey
  , stopSystemTray
  ) where

import Data.Text (Text)
import Haskellite.Types (HotkeyPreset (..))
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
type SystemTray = Tray.SystemTray

startGlobalHotkey :: HotkeyPreset -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey = Native.startGlobalHotkey

stopGlobalHotkey :: GlobalHotkey -> IO ()
stopGlobalHotkey = Native.stopGlobalHotkey

sendPasteShortcut :: IO (Either Text ())
sendPasteShortcut = Native.sendPasteShortcut

startSystemTray :: SDL.Window -> IO () -> IO () -> IO (Either Text SystemTray)
startSystemTray = Tray.startSystemTray

stopSystemTray :: SystemTray -> IO ()
stopSystemTray = Tray.stopSystemTray

hotkeyLabel :: HotkeyPreset -> Text
hotkeyLabel preset = case preset of
  ControlShiftSpace -> "Ctrl + Shift + Space"
  ControlAltSpace -> "Ctrl + Alt + Space"
  SuperShiftSpace -> "Super + Shift + Space"
  FunctionKey8 -> "F8"
  FunctionKey9 -> "F9"
