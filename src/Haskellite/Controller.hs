{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Controller
  ( Session
  , SessionEvent (..)
  , startSession
  , stopSession
  ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.STM
  ( TBQueue
  , TVar
  , atomically
  , newTBQueueIO
  , newTVarIO
  , readTBQueue
  , readTVar
  , writeTBQueue
  , writeTVar
  )
import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Haskellite.Audio
  ( AudioCapture
  , captureSampleRate
  , readAudioChunk
  , startAudioCapture
  , stopAudioCapture
  )
import Haskellite.Parakeet (Parakeet, transcribeSamples)
import Haskellite.Types
  ( RecognitionResult (recognizedLanguage, recognizedText)
  , Settings (..)
  , TranscriptSegment (..)
  )
import Haskellite.VAD
  ( VadConfig (..)
  , VadEvent (..)
  , feedAudio
  , flushAudio
  , initialVadState
  )

data SessionEvent
  = InputLevel Float
  | SpeechBegan
  | SpeechEnded
  | TranscriptionBegan
  | TranscriptionCompleted TranscriptSegment
  | SessionFailed Text
  deriving (Eq, Show)

data AudioJob = AudioJob
  { jobSampleRate :: Int
  , jobSamples :: Vector Float
  }

data Session = Session
  { sessionStopSignal :: TVar Bool
  , audioWorker :: Async ()
  , recognitionWorker :: Async ()
  }

startSession :: Parakeet -> Settings -> (SessionEvent -> IO ()) -> IO Session
startSession parakeet settings notify = do
  capture <- startAudioCapture (audioDeviceName settings)
  stopSignal <- newTVarIO False
  jobs <- newTBQueueIO 8
  recognizer <- async $ recognitionLoop parakeet jobs notify
  recorder <- async $ do
    outcome <- try $ audioLoop capture settings stopSignal jobs notify
    case outcome of
      Left exception -> notify . SessionFailed . Text.pack $ displayException (exception :: SomeException)
      Right () -> pure ()
  pure
    Session
      { sessionStopSignal = stopSignal
      , audioWorker = recorder
      , recognitionWorker = recognizer
      }

stopSession :: Session -> IO ()
stopSession Session {sessionStopSignal, audioWorker, recognitionWorker} = do
  atomically $ writeTVar sessionStopSignal True
  _ <- wait audioWorker
  _ <- wait recognitionWorker
  pure ()

audioLoop :: AudioCapture -> Settings -> TVar Bool -> TBQueue (Maybe AudioJob) -> (SessionEvent -> IO ()) -> IO ()
audioLoop capture settings stopSignal jobs notify =
  go initialVadState
    `finally` (stopAudioCapture capture `finally` atomically (writeTBQueue jobs Nothing))
  where
    sampleRate = captureSampleRate capture
    config =
      VadConfig
        { vadSampleRate = sampleRate
        , vadThresholdDb = voiceThresholdDb settings
        , vadMinimumSpeechMs = 180
        , vadTrailingSilenceMs = trailingSilenceMs settings
        , vadPreRollMs = 250
        , vadMaximumUtteranceMs = maximumUtteranceSeconds settings * 1000
        }

    go state = do
      next <- atomically $ do
        stopped <- readTVar stopSignal
        if stopped then pure Nothing else Just <$> readAudioChunk capture
      case next of
        Nothing -> do
          let (_, events) = flushAudio config state
          dispatchVadEvents sampleRate jobs notify events
        Just samples -> do
          let (newState, events) = feedAudio config state samples
          dispatchVadEvents sampleRate jobs notify events
          go newState

dispatchVadEvents :: Int -> TBQueue (Maybe AudioJob) -> (SessionEvent -> IO ()) -> [VadEvent] -> IO ()
dispatchVadEvents sampleRate jobs notify = mapM_ dispatch
  where
    dispatch (LevelChanged level) = notify (InputLevel level)
    dispatch VoiceStarted = notify SpeechBegan
    dispatch (UtteranceReady samples) = do
      notify SpeechEnded
      atomically . writeTBQueue jobs . Just $ AudioJob sampleRate samples

recognitionLoop :: Parakeet -> TBQueue (Maybe AudioJob) -> (SessionEvent -> IO ()) -> IO ()
recognitionLoop parakeet jobs notify = go 1
  where
    go segmentNumber = do
      next <- atomically $ readTBQueue jobs
      case next of
        Nothing -> pure ()
        Just AudioJob {jobSampleRate, jobSamples} -> do
          notify TranscriptionBegan
          result <- try $ transcribeSamples parakeet jobSampleRate jobSamples
          case result of
            Left exception -> notify . SessionFailed . Text.pack $ displayException (exception :: SomeException)
            Right recognized ->
              unless (Text.null $ Text.strip $ recognizedText recognized) $
                notify . TranscriptionCompleted $
                  TranscriptSegment
                    { segmentNumber
                    , segmentText = Text.strip (recognizedText recognized)
                    , segmentLanguage = recognizedLanguage recognized
                    , segmentAudioSeconds = fromIntegral (Vector.length jobSamples) / fromIntegral jobSampleRate
                    }
          go (segmentNumber + 1)
