{-# LANGUAGE StrictData #-}

module Haskellite.VAD
  ( VadConfig (..)
  , VadEvent (..)
  , VadState
  , feedAudio
  , flushAudio
  , initialVadState
  , rmsDecibels
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq ((:<|), Empty))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable (Vector)

data VadConfig = VadConfig
  { vadSampleRate :: Int
  , vadThresholdDb :: Float
  , vadMinimumSpeechMs :: Int
  , vadTrailingSilenceMs :: Int
  , vadPreRollMs :: Int
  , vadMaximumUtteranceMs :: Int
  }
  deriving (Eq, Show)

data VadEvent
  = LevelChanged Float
  | VoiceStarted
  | UtteranceReady (Vector Float)
  deriving (Eq, Show)

data VadMode
  = Waiting
  | Speaking
  deriving (Eq, Show)

data VadState = VadState
  { mode :: VadMode
  , frames :: Seq (Vector Float)
  , frameSamples :: Int
  , speechSamples :: Int
  , silenceSamples :: Int
  }
  deriving (Eq, Show)

initialVadState :: VadState
initialVadState = VadState Waiting Seq.empty 0 0 0

feedAudio :: VadConfig -> VadState -> Vector Float -> (VadState, [VadEvent])
feedAudio config state chunk
  | Vector.null chunk = (state, [])
  | otherwise =
      case mode state of
        Waiting -> feedWaiting
        Speaking -> feedSpeaking
  where
    db = rmsDecibels chunk
    isVoice = db >= vadThresholdDb config
    levelEvent = LevelChanged (meterLevel db)
    chunkSamples = Vector.length chunk

    feedWaiting
      | isVoice =
          let withChunk = appendFrame state chunk
              started =
                withChunk
                  { mode = Speaking
                  , speechSamples = chunkSamples
                  , silenceSamples = 0
                  }
           in (started, [levelEvent, VoiceStarted])
      | otherwise =
          let buffered = trimToSamples (samplesForMs config (vadPreRollMs config)) (appendFrame state chunk)
           in (buffered, [levelEvent])

    feedSpeaking =
      let appended = appendFrame state chunk
          updated =
            if isVoice
              then
                appended
                  { speechSamples = speechSamples state + chunkSamples
                  , silenceSamples = 0
                  }
              else appended {silenceSamples = silenceSamples state + chunkSamples}
          longEnough = speechSamples updated >= samplesForMs config (vadMinimumSpeechMs config)
          reachedPause = silenceSamples updated >= samplesForMs config (vadTrailingSilenceMs config)
          reachedLimit = frameSamples updated >= samplesForMs config (vadMaximumUtteranceMs config)
       in if reachedLimit || (longEnough && reachedPause)
            then (initialVadState, [levelEvent, UtteranceReady (joinFrames updated)])
            else
              if reachedPause
                then
                  let carry = trimToSamples (samplesForMs config (vadPreRollMs config)) updated
                   in (carry {mode = Waiting, speechSamples = 0, silenceSamples = 0}, [levelEvent])
                else (updated, [levelEvent])

flushAudio :: VadConfig -> VadState -> (VadState, [VadEvent])
flushAudio config state
  | mode state == Speaking
      && speechSamples state >= samplesForMs config (vadMinimumSpeechMs config) =
      (initialVadState, [UtteranceReady (joinFrames state)])
  | otherwise = (initialVadState, [])

rmsDecibels :: Vector Float -> Float
rmsDecibels samples
  | Vector.null samples = -120
  | otherwise =
      let squares = Vector.foldl' (\acc sample -> acc + sample * sample) 0 samples
          rms = sqrt (squares / fromIntegral (Vector.length samples))
       in if rms <= 1.0e-6 then -120 else 20 * logBase 10 rms

meterLevel :: Float -> Float
meterLevel db = max 0 (min 1 ((db + 60) / 60))

samplesForMs :: VadConfig -> Int -> Int
samplesForMs config milliseconds = vadSampleRate config * milliseconds `div` 1000

appendFrame :: VadState -> Vector Float -> VadState
appendFrame state chunk =
  state
    { frames = frames state Seq.|> chunk
    , frameSamples = frameSamples state + Vector.length chunk
    }

trimToSamples :: Int -> VadState -> VadState
trimToSamples target state
  | frameSamples state <= target = state
  | otherwise = go state
  where
    go current
      | frameSamples current <= target = current
      | otherwise =
          case frames current of
            Empty -> current {frameSamples = 0}
            first :<| rest ->
              let firstSize = Vector.length first
                  excess = frameSamples current - target
               in if firstSize <= excess
                    then go current {frames = rest, frameSamples = frameSamples current - firstSize}
                    else
                      current
                        { frames = Vector.drop excess first Seq.<| rest
                        , frameSamples = target
                        }

joinFrames :: VadState -> Vector Float
joinFrames = Vector.concat . toList . frames
