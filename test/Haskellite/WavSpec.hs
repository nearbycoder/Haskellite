{-# LANGUAGE OverloadedStrings #-}

module Haskellite.WavSpec (wavTests) where

import Data.ByteString qualified as ByteString
import Data.Vector.Storable qualified as Vector
import Haskellite.Wav (WavAudio (..), decodeWav, encodeWav16)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

wavTests :: TestTree
wavTests =
  testGroup
    "WAV codec"
    [ testCase "PCM16 round trip preserves rate and samples" $ do
        let original = Vector.fromList [-1, -0.5, 0, 0.5, 1]
        case decodeWav (encodeWav16 16000 original) of
          Left message -> assertFailure (show message)
          Right WavAudio {wavSampleRate, wavSamples} -> do
            wavSampleRate @?= 16000
            Vector.length wavSamples @?= Vector.length original
            assertBool "quantization error is small" (Vector.and $ Vector.zipWith (\a b -> abs (a - b) < 0.0001) original wavSamples)
    , testCase "invalid input is rejected" $
        assertBool "decode failed" (case decodeWav "not a wave" of Left _ -> True; Right _ -> False)
    , testCase "encoded RIFF size matches file size" $ do
        let encoded = encodeWav16 16000 (Vector.replicate 160 0)
        ByteString.length encoded @?= 44 + 320
    ]
