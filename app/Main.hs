{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, bracket, displayException, finally, try)
import Control.Concurrent.STM (atomically)
import Control.Monad (when)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Vector.Storable qualified as Vector
import Haskellite.Audio
  ( captureSampleRate
  , captureHealth
  , listCaptureDevices
  , readAudioChunk
  , startAudioCapture
  , stopAudioCapture
  )
import Haskellite.Parakeet (transcribeSamples, withParakeet)
import Haskellite.Runtime
  ( DownloadAsset (assetName)
  , currentRuntimeAsset
  , discoverAppPaths
  , installParakeetFor
  , loadSettings
  , resolveModelPathsFor
  , resolveRuntimeLibraries
  , runtimeVersion
  )
import Haskellite.Types
  ( AppPaths
  , InstallProgress (..)
  , RecognitionResult (recognizedText)
  , Settings (inferenceThreads, selectedModelId)
  )
import Haskellite.UI (runDesktop)
import Haskellite.Wav (WavAudio (..), readWavFile)
import Options.Applicative
  ( Parser
  , ParserInfo
  , command
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , metavar
  , optional
  , progDesc
  , strArgument
  , (<**>)
  )
import System.Exit (die)
import System.Timeout (timeout)
import Text.Printf (printf)
import qualified SDL

data Command
  = Desktop
  | Install
  | Diagnose
  | CheckMicrophone
  | Transcribe FilePath

main :: IO ()
main = do
  commandToRun <- execParser options
  appPaths <- discoverAppPaths
  run <- try $ case commandToRun of
    Desktop -> runDesktop appPaths
    Install -> do
      settings <- loadSettings appPaths
      installParakeetFor appPaths (selectedModelId settings) printProgress
    Diagnose -> diagnostics appPaths
    CheckMicrophone -> checkMicrophone
    Transcribe path -> transcribeFile appPaths path
  case run of
    Left exception -> die (displayException (exception :: SomeException))
    Right () -> pure ()

options :: ParserInfo Command
options =
  info
    (commandParser <**> helper)
    (fullDesc <> header "Haskellite — local NVIDIA Parakeet voice transcription")

commandParser :: Parser Command
commandParser =
  maybe Desktop id
    <$> optional
      ( hsubparser
          ( command "install" (info (pure Install) (progDesc "Download and verify the Parakeet model and runtime"))
              <> command "diagnostics" (info (pure Diagnose) (progDesc "Check the local runtime and model installation"))
              <> command "check-microphone" (info (pure CheckMicrophone) (progDesc "Open the default microphone and verify audio frames arrive"))
              <> command "transcribe" (info transcribeParser (progDesc "Transcribe a PCM WAV file without opening the UI"))
          )
      )
  where
    transcribeParser = Transcribe <$> strArgument (metavar "AUDIO.wav" <> help "Mono or multi-channel PCM16/PCM32/Float32 WAV file")

printProgress :: InstallProgress -> IO ()
printProgress progress =
  putStrLn $ Text.unpack (installMessage progress) <> maybe "" renderBytes (installBytesTotal progress)
  where
    renderBytes total = printf " (%.1f%%)" (100 * fromIntegral (installBytesComplete progress) / fromIntegral total :: Double)

diagnostics :: AppPaths -> IO ()
diagnostics appPaths = do
  settings <- loadSettings appPaths
  runtime <- resolveRuntimeLibraries appPaths
  model <- resolveModelPathsFor appPaths (selectedModelId settings)
  putStrLn $ "Haskellite runtime: " <> Text.unpack runtimeVersion
  putStrLn $ "Parakeet model:     " <> Text.unpack (selectedModelId settings)
  putStrLn $ "Platform asset:     " <> either Text.unpack assetName currentRuntimeAsset
  putStrLn $ "Runtime files:      " <> either Text.unpack (const "ready") runtime
  putStrLn $ "Model files:        " <> either Text.unpack (const "ready") model

transcribeFile :: AppPaths -> FilePath -> IO ()
transcribeFile appPaths path = do
  settings <- loadSettings appPaths
  runtime <- resolveRuntimeLibraries appPaths >>= either (die . Text.unpack) pure
  model <- resolveModelPathsFor appPaths (selectedModelId settings) >>= either (die . Text.unpack) pure
  WavAudio sampleRate samples <- readWavFile path >>= either (die . Text.unpack) pure
  when (Vector.null samples) $ die "The WAV file has no audio samples"
  withParakeet runtime model (inferenceThreads settings) $ \parakeet -> do
    result <- transcribeSamples parakeet sampleRate samples
    TextIO.putStrLn (recognizedText result)

checkMicrophone :: IO ()
checkMicrophone = do
  SDL.initialize [SDL.InitAudio]
  ( do
      names <- listCaptureDevices
      putStrLn $ "Capture devices: " <> show (fmap Text.unpack names)
      bracket (startAudioCapture Nothing) stopAudioCapture $ \capture -> do
        received <- timeout 5000000 (atomically $ readAudioChunk capture)
        health <- captureHealth capture
        case received of
          Nothing -> die $ "The microphone opened, but no audio frames arrived within five seconds. SDL status/queued bytes/worker: " <> show health
          Just samples ->
            putStrLn $ "Microphone ready: " <> show (captureSampleRate capture) <> " Hz, " <> show (Vector.length samples) <> " samples received"
    )
    `finally` SDL.quit
