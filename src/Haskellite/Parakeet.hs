{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Parakeet
  ( Parakeet
  , closeParakeet
  , openParakeet
  , transcribeSamples
  , withParakeet
  ) where

import Control.Exception (bracket, onException)
import Data.Vector.Storable (Vector)
import Haskellite.Internal.Sherpa
  ( SherpaApi
  , SherpaRecognizer
  , closeSherpaApi
  , createSherpaRecognizer
  , decodeSamples
  , destroySherpaRecognizer
  , loadSherpaApi
  )
import Haskellite.Runtime (RuntimeLibraries (..))
import Haskellite.Types (ModelPaths, RecognitionResult)

data Parakeet = Parakeet
  { parakeetApi :: SherpaApi
  , parakeetRecognizer :: SherpaRecognizer
  }

openParakeet :: RuntimeLibraries -> ModelPaths -> Int -> IO Parakeet
openParakeet RuntimeLibraries {runtimeApiLibrary, runtimeDependencies} modelPaths threads = do
  api <- loadSherpaApi runtimeDependencies runtimeApiLibrary
  recognizer <- createSherpaRecognizer api modelPaths threads `onException` closeSherpaApi api
  pure Parakeet {parakeetApi = api, parakeetRecognizer = recognizer}

closeParakeet :: Parakeet -> IO ()
closeParakeet Parakeet {parakeetApi, parakeetRecognizer} = do
  destroySherpaRecognizer parakeetApi parakeetRecognizer
  closeSherpaApi parakeetApi

withParakeet :: RuntimeLibraries -> ModelPaths -> Int -> (Parakeet -> IO a) -> IO a
withParakeet runtime model threads = bracket (openParakeet runtime model threads) closeParakeet

transcribeSamples :: Parakeet -> Int -> Vector Float -> IO RecognitionResult
transcribeSamples Parakeet {parakeetApi, parakeetRecognizer} = decodeSamples parakeetApi parakeetRecognizer
