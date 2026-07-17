{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Platform.Windows
  ( GlobalHotkey
  , PasteTarget
  , capturePasteTarget
  , sendPasteShortcut
  , startGlobalHotkey
  , stopGlobalHotkey
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void, when)
import Data.Bits ((.&.))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8, Word32)
import Foreign.C.Types (CShort (..))
import Foreign.Ptr (WordPtr (..))
import Haskellite.Types
  ( HotkeyKey (..)
  , HotkeyModifiers (..)
  , HotkeyPreset
  , hotkeyBinding
  )

newtype GlobalHotkey = GlobalHotkey (Async ())

data PasteTarget = PasteTarget

capturePasteTarget :: IO (Maybe PasteTarget)
capturePasteTarget = pure (Just PasteTarget)

startGlobalHotkey :: HotkeyPreset -> IO () -> IO () -> IO (Either Text GlobalHotkey)
startGlobalHotkey preset pressedAction releasedAction = do
  worker <- async $ poll False
  pure . Right $ GlobalHotkey worker
  where
    poll wasPressed = do
      pressed <- hotkeyPressed preset
      when (pressed && not wasPressed) pressedAction
      when (not pressed && wasPressed) releasedAction
      threadDelay 16000
      poll pressed

stopGlobalHotkey :: GlobalHotkey -> IO ()
stopGlobalHotkey (GlobalHotkey worker) = cancel worker >> void (waitCatch worker)

sendPasteShortcut :: Maybe PasteTarget -> IO (Either Text ())
sendPasteShortcut _ = do
  outcome <- try $ do
    keyEvent virtualControl 0
    keyEvent virtualV 0
    keyEvent virtualV keyUp
    keyEvent virtualControl keyUp
  pure $ either (Left . Text.pack . displayException) Right (outcome :: Either SomeException ())

hotkeyPressed :: HotkeyPreset -> IO Bool
hotkeyPressed preset = allM isDown (hotkeyKeys preset)

hotkeyKeys :: HotkeyPreset -> [Word8]
hotkeyKeys preset = modifierKeys <> [windowsKey key]
  where
    (modifiers, key) = hotkeyBinding preset
    modifierKeys =
      concat
        [ [virtualControl | modifierControl modifiers]
        , [virtualShift | modifierShift modifiers]
        , [virtualAlt | modifierAlt modifiers]
        , [virtualWindows | modifierSuper modifiers]
        ]

windowsKey :: HotkeyKey -> Word8
windowsKey key
  | key >= HotkeyA && key <= HotkeyZ = fromIntegral $ 0x41 + fromEnum key - fromEnum HotkeyA
  | key >= Hotkey0 && key <= Hotkey9 = fromIntegral $ 0x30 + fromEnum key - fromEnum Hotkey0
  | key >= HotkeyF1 && key <= HotkeyF12 = fromIntegral $ 0x70 + fromEnum key - fromEnum HotkeyF1
  | otherwise = case key of
      HotkeySpace -> 0x20
      HotkeyTab -> 0x09
      HotkeyReturn -> 0x0D
      HotkeyEscape -> 0x1B
      HotkeyBackspace -> 0x08
      HotkeyLeft -> 0x25
      HotkeyRight -> 0x27
      HotkeyUp -> 0x26
      HotkeyDown -> 0x28
      HotkeyHome -> 0x24
      HotkeyEnd -> 0x23
      HotkeyPageUp -> 0x21
      HotkeyPageDown -> 0x22
      HotkeyInsert -> 0x2D
      HotkeyDelete -> 0x2E
      HotkeyMinus -> 0xBD
      HotkeyEquals -> 0xBB
      HotkeyLeftBracket -> 0xDB
      HotkeyRightBracket -> 0xDD
      HotkeyBackslash -> 0xDC
      HotkeySemicolon -> 0xBA
      HotkeyQuote -> 0xDE
      HotkeyBackquote -> 0xC0
      HotkeyComma -> 0xBC
      HotkeyPeriod -> 0xBE
      HotkeySlash -> 0xBF
      _ -> 0

isDown :: Word8 -> IO Bool
isDown virtualKey = do
  state <- getAsyncKeyState (fromIntegral virtualKey)
  pure $ (fromIntegral state :: Word32) .&. 0x8000 /= 0

keyEvent :: Word8 -> Word32 -> IO ()
keyEvent virtualKey flags = keybdEvent virtualKey 0 flags 0

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM predicate = go
  where
    go [] = pure True
    go (value : rest) = do
      matches <- predicate value
      if matches then go rest else pure False

virtualControl, virtualShift, virtualAlt, virtualWindows, virtualV :: Word8
virtualControl = 0x11
virtualShift = 0x10
virtualAlt = 0x12
virtualWindows = 0x5B
virtualV = 0x56

keyUp :: Word32
keyUp = 0x0002

foreign import stdcall unsafe "GetAsyncKeyState"
  getAsyncKeyState :: Word32 -> IO CShort

foreign import stdcall unsafe "keybd_event"
  keybdEvent :: Word8 -> Word8 -> Word32 -> WordPtr -> IO ()
