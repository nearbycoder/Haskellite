{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Audio
  ( AudioCapture
  , AudioCue (..)
  , captureHealth
  , captureSampleRate
  , listCaptureDevices
  , playAudioCue
  , readAudioChunk
  , startAudioCapture
  , stopAudioCapture
  , tryReadAudioChunk
  ) where

#include <SDL.h>

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, poll, wait)
import Control.Concurrent.STM
  ( STM
  , TBQueue
  , TVar
  , atomically
  , isFullTBQueue
  , newTBQueueIO
  , newTVarIO
  , readTBQueue
  , readTVarIO
  , tryReadTBQueue
  , writeTBQueue
  , writeTVar
  )
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.ByteString qualified as ByteString
import Data.Foldable qualified as Foldable
import Data.Text (Text)
import Data.Text qualified as TextValue
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Data.Word (Word16, Word32, Word8)
import Foreign
  ( Ptr
  , allocaBytesAligned
  , castPtr
  , nullPtr
  , peekByteOff
  , peekElemOff
  , pokeByteOff
  )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Utils (fillBytes)
import qualified SDL

type AudioDeviceId = Word32

foreign import ccall unsafe "SDL_OpenAudioDevice"
  cOpenAudioDevice :: CString -> CInt -> Ptr () -> Ptr () -> CInt -> IO AudioDeviceId

foreign import ccall unsafe "SDL_PauseAudioDevice"
  cPauseAudioDevice :: AudioDeviceId -> CInt -> IO ()

foreign import ccall unsafe "SDL_GetQueuedAudioSize"
  cGetQueuedAudioSize :: AudioDeviceId -> IO Word32

foreign import ccall unsafe "SDL_GetAudioDeviceStatus"
  cGetAudioDeviceStatus :: AudioDeviceId -> IO CInt

foreign import ccall unsafe "SDL_DequeueAudio"
  cDequeueAudio :: AudioDeviceId -> Ptr () -> Word32 -> IO Word32

foreign import ccall unsafe "SDL_CloseAudioDevice"
  cCloseAudioDevice :: AudioDeviceId -> IO ()

foreign import ccall unsafe "SDL_GetError"
  cGetError :: IO CString

foreign import ccall unsafe "SDL_QueueAudio"
  cQueueAudio :: AudioDeviceId -> Ptr () -> Word32 -> IO CInt

data AudioCue = DictationStarted | DictationCompleted
  deriving (Eq, Show)

data AudioCapture = AudioCapture
  { captureDevice :: AudioDeviceId
  , captureQueue :: TBQueue (Vector Float)
  , captureSampleRate :: Int
  , captureStop :: TVar Bool
  , captureWorker :: Async ()
  }

listCaptureDevices :: IO [Text]
listCaptureDevices = Foldable.toList . maybe mempty id <$> SDL.getAudioDeviceNames SDL.ForCapture

playAudioCue :: AudioCue -> IO ()
playAudioCue cue =
  allocaBytesAligned audioSpecSize audioSpecAlignment $ \desired ->
    allocaBytesAligned audioSpecSize audioSpecAlignment $ \obtained -> do
      fillBytes desired 0 audioSpecSize
      fillBytes obtained 0 audioSpecSize
      pokeByteOff desired frequencyOffset (fromIntegral cueSampleRate :: CInt)
      pokeByteOff desired formatOffset floatingNativeFormat
      pokeByteOff desired channelsOffset (1 :: Word8)
      pokeByteOff desired samplesOffset (512 :: Word16)
      device <- cOpenAudioDevice nullPtr 0 desired obtained 0
      when (device == 0) $ sdlError "Could not open the audio cue device"
      let samples = cueSamples cue
          byteCount = Vector.length samples * sizeOfFloat
      queued <- Vector.unsafeWith samples $ \buffer -> cQueueAudio device (castPtr buffer) (fromIntegral byteCount)
      when (queued /= 0) $ cCloseAudioDevice device >> sdlError "Could not queue the audio cue"
      cPauseAudioDevice device 0
      threadDelay (cueDurationMs * 1000 + 30000)
      cCloseAudioDevice device

cueSamples :: AudioCue -> Vector Float
cueSamples cue = Vector.generate sampleCount sampleAt
  where
    sampleCount = cueSampleRate * cueDurationMs `div` 1000
    half = sampleCount `div` 2
    (firstFrequency, secondFrequency) = case cue of
      DictationStarted -> (660, 880)
      DictationCompleted -> (880, 660)
    sampleAt index =
      let frequency = if index < half then firstFrequency else secondFrequency
          phase = 2 * pi * frequency * fromIntegral index / fromIntegral cueSampleRate
          edge = min index (sampleCount - index - 1)
          envelope = min 1 (fromIntegral edge / 160)
       in realToFrac (0.12 * envelope * sin phase :: Double)

cueSampleRate, cueDurationMs :: Int
cueSampleRate = 44100
cueDurationMs = 120

startAudioCapture :: Maybe Text -> IO AudioCapture
startAudioCapture requestedDevice = do
  queue <- newTBQueueIO 64
  stopSignal <- newTVarIO False
  (device, actualRate) <- withDeviceName requestedDevice $ \deviceName ->
    allocaBytesAligned audioSpecSize audioSpecAlignment $ \desired ->
      allocaBytesAligned audioSpecSize audioSpecAlignment $ \obtained -> do
        fillBytes desired 0 audioSpecSize
        fillBytes obtained 0 audioSpecSize
        pokeByteOff desired frequencyOffset (16000 :: CInt)
        pokeByteOff desired formatOffset floatingNativeFormat
        pokeByteOff desired channelsOffset (1 :: Word8)
        pokeByteOff desired samplesOffset (1024 :: Word16)
        opened <- cOpenAudioDevice deviceName 1 desired obtained 0
        when (opened == 0) $ sdlError "Could not open the microphone"
        actualFormat <- peekByteOff obtained formatOffset
        actualChannels <- peekByteOff obtained channelsOffset
        actualFrequency <- peekByteOff obtained frequencyOffset
        unless (actualFormat == floatingNativeFormat && actualChannels == (1 :: Word8)) $ do
          cCloseAudioDevice opened
          throwIO . userError $ "SDL did not provide mono float audio."
        pure (opened, fromIntegral (actualFrequency :: CInt))
  cPauseAudioDevice device 0
  worker <- async $ dequeueLoop device queue stopSignal
  pure
    AudioCapture
      { captureDevice = device
      , captureQueue = queue
      , captureSampleRate = actualRate
      , captureStop = stopSignal
      , captureWorker = worker
      }

stopAudioCapture :: AudioCapture -> IO ()
stopAudioCapture AudioCapture {captureDevice, captureStop, captureWorker} = do
  atomically $ writeTVar captureStop True
  _ <- wait captureWorker
  cPauseAudioDevice captureDevice 1
  cCloseAudioDevice captureDevice

readAudioChunk :: AudioCapture -> STM (Vector Float)
readAudioChunk = readTBQueue . captureQueue

tryReadAudioChunk :: AudioCapture -> STM (Maybe (Vector Float))
tryReadAudioChunk = tryReadTBQueue . captureQueue

captureHealth :: AudioCapture -> IO (Int, Int, Maybe Text)
captureHealth AudioCapture {captureDevice, captureWorker} = do
  status <- cGetAudioDeviceStatus captureDevice
  queued <- cGetQueuedAudioSize captureDevice
  worker <- poll captureWorker
  let workerState = case worker of
        Nothing -> Nothing
        Just (Right ()) -> Just "capture worker stopped"
        Just (Left exception) -> Just (TextValue.pack $ show exception)
  pure (fromIntegral status, fromIntegral queued, workerState)

dequeueLoop :: AudioDeviceId -> TBQueue (Vector Float) -> TVar Bool -> IO ()
dequeueLoop device queue stopSignal = go
  where
    targetBytes = 1024 * sizeOfFloat

    go = do
      stopped <- readTVarIO stopSignal
      unless stopped $ do
        available <- fromIntegral <$> cGetQueuedAudioSize device
        if available < targetBytes
          then threadDelay 5000
          else do
            let requestedBytes = min available (targetBytes * 4)
                alignedBytes = requestedBytes - (requestedBytes `mod` sizeOfFloat)
            allocaBytesAligned alignedBytes #{alignment float} $ \buffer -> do
              receivedBytes <- fromIntegral <$> cDequeueAudio device buffer (fromIntegral alignedBytes)
              let receivedSamples = receivedBytes `div` sizeOfFloat
              samples <- Vector.generateM receivedSamples (peekElemOff (castPtr buffer :: Ptr Float))
              unless (Vector.null samples) $ atomically do
                full <- isFullTBQueue queue
                unless full $ writeTBQueue queue samples
        go

withDeviceName :: Maybe Text -> (CString -> IO a) -> IO a
withDeviceName Nothing action = action nullPtr
withDeviceName (Just name) action = ByteString.useAsCString (Text.encodeUtf8 name) action

sdlError :: String -> IO a
sdlError prefix = do
  pointer <- cGetError
  bytes <- ByteString.packCString pointer
  let message = Text.decodeUtf8With lenientDecode bytes
  throwIO . userError $ prefix <> ": " <> TextValue.unpack message

audioSpecSize, audioSpecAlignment :: Int
audioSpecSize = #{size SDL_AudioSpec}
audioSpecAlignment = #{alignment SDL_AudioSpec}

frequencyOffset, formatOffset, channelsOffset, samplesOffset :: Int
frequencyOffset = #{offset SDL_AudioSpec, freq}
formatOffset = #{offset SDL_AudioSpec, format}
channelsOffset = #{offset SDL_AudioSpec, channels}
samplesOffset = #{offset SDL_AudioSpec, samples}

floatingNativeFormat :: Word16
floatingNativeFormat = #{const AUDIO_F32SYS}

sizeOfFloat :: Int
sizeOfFloat = #{size float}
