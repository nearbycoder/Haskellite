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
import Haskellite.Types (HotkeyPreset (..))

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
hotkeyKeys preset = case preset of
  ControlShiftSpace -> [virtualControl, virtualShift, virtualSpace]
  ControlAltSpace -> [virtualControl, virtualAlt, virtualSpace]
  SuperShiftSpace -> [virtualWindows, virtualShift, virtualSpace]
  FunctionKey8 -> [virtualF8]
  FunctionKey9 -> [virtualF9]

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

virtualControl, virtualShift, virtualAlt, virtualWindows, virtualSpace, virtualV, virtualF8, virtualF9 :: Word8
virtualControl = 0x11
virtualShift = 0x10
virtualAlt = 0x12
virtualWindows = 0x5B
virtualSpace = 0x20
virtualV = 0x56
virtualF8 = 0x77
virtualF9 = 0x78

keyUp :: Word32
keyUp = 0x0002

foreign import stdcall unsafe "GetAsyncKeyState"
  getAsyncKeyState :: Word32 -> IO CShort

foreign import stdcall unsafe "keybd_event"
  keybdEvent :: Word8 -> Word8 -> Word32 -> WordPtr -> IO ()
