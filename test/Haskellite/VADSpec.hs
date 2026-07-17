module Haskellite.VADSpec (vadTests) where

import Data.Vector.Storable qualified as Vector
import Haskellite.VAD
  ( VadConfig (..)
  , VadEvent (..)
  , feedAudio
  , flushAudio
  , initialVadState
  , rmsDecibels
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

vadTests :: TestTree
vadTests =
  testGroup
    "voice activity detection"
    [ testCase "silence remains silent" $ do
        let (_, events) = feedAudio config initialVadState (Vector.replicate 100 0)
        assertBool "no voice start" (VoiceStarted `notElem` events)
    , testCase "speech followed by a pause emits one utterance" $ do
        let chunks = replicate 3 (Vector.replicate 100 0.2) <> replicate 3 (Vector.replicate 100 0)
            (_, events) = feedMany initialVadState chunks
            utterances = [samples | UtteranceReady samples <- events]
        case utterances of
          [utterance] -> assertBool "pre-roll and trailing audio are retained" (Vector.length utterance >= 500)
          _ -> length utterances @?= 1
    , testCase "flush emits the final spoken phrase" $ do
        let (speaking, _) = feedMany initialVadState (replicate 3 $ Vector.replicate 100 0.2)
            (_, events) = flushAudio config speaking
        assertBool "utterance emitted" (any isUtterance events)
    , testCase "RMS is expressed in dBFS" $ do
        assertBool "full scale is approximately zero dB" (abs (rmsDecibels $ Vector.replicate 100 1) < 0.001)
        assertBool "silence is floored" (rmsDecibels (Vector.replicate 100 0) <= -100)
    ]
  where
    feedMany initial chunks = foldl step (initial, []) chunks
    step (state, accumulated) chunk =
      let (next, events) = feedAudio config state chunk
       in (next, accumulated <> events)
    isUtterance (UtteranceReady _) = True
    isUtterance _ = False

config :: VadConfig
config =
  VadConfig
    { vadSampleRate = 1000
    , vadThresholdDb = -40
    , vadMinimumSpeechMs = 180
    , vadTrailingSilenceMs = 300
    , vadPreRollMs = 200
    , vadMaximumUtteranceMs = 10000
    }
