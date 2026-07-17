{-# LANGUAGE OverloadedStrings #-}

module Haskellite.HistorySpec (historyTests) where

import Haskellite.History (appendHistory, historyFilePath, loadHistory)
import Haskellite.Types
  ( ActivationSource (HotkeyActivation)
  , AppPaths (..)
  , TranscriptRecord (..)
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)
import System.Directory (doesFileExist)

historyTests :: TestTree
historyTests =
  testGroup
    "dictation history"
    [ testCase "missing history loads as empty" $ withPaths $ \paths ->
        loadHistory paths >>= (@?= Right [])
    , testCase "records survive an append/load round trip" $ withPaths $ \paths -> do
        appendHistory paths firstRecord
        appendHistory paths secondRecord
        loadHistory paths >>= (@?= Right [firstRecord, secondRecord])
        assertBool "history file exists" =<< doesFileExist (historyFilePath paths)
    ]

withPaths :: (AppPaths -> IO a) -> IO a
withPaths action = withSystemTempDirectory "haskellite-history" $ \directory ->
  action
    AppPaths
      { appDataDirectory = directory
      , settingsFile = directory </> "settings.json"
      , runtimeDirectory = directory </> "runtime"
      , modelDirectory = directory </> "models"
      , transcriptDirectory = directory </> "transcripts"
      }

firstRecord, secondRecord :: TranscriptRecord
firstRecord =
  TranscriptRecord
    { recordId = "20260716-0001"
    , recordStartedAt = "2026-07-16T12:00:00Z"
    , recordCompletedAt = "2026-07-16T12:00:04Z"
    , recordSource = HotkeyActivation
    , recordModelId = "parakeet-tdt-0.6b-v3-int8"
    , recordText = "The first dictated phrase."
    , recordLanguage = Just "en"
    , recordAudioSeconds = 3.5
    , recordInjected = True
    }

secondRecord = firstRecord {recordId = "20260716-0002", recordText = "Déjà vu."}
