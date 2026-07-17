{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Types
  ( AppPaths (..)
  , ActivationSource (..)
  , HotkeyPreset (..)
  , InstallProgress (..)
  , InstallStage (..)
  , ModelPaths (..)
  , RecognitionResult (..)
  , Settings (..)
  , TranscriptRecord (..)
  , TranscriptSegment (..)
  , defaultSettings
  , emptyRecognitionResult
  , renderTranscript
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data AppPaths = AppPaths
  { appDataDirectory :: FilePath
  , settingsFile :: FilePath
  , runtimeDirectory :: FilePath
  , modelDirectory :: FilePath
  , transcriptDirectory :: FilePath
  }
  deriving stock (Eq, Show, Generic)

data ModelPaths = ModelPaths
  { encoderPath :: FilePath
  , decoderPath :: FilePath
  , joinerPath :: FilePath
  , tokensPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)

data InstallStage
  = CheckingFiles
  | DownloadingRuntime
  | VerifyingRuntime
  | ExtractingRuntime
  | DownloadingModel
  | VerifyingModel
  | ExtractingModel
  | InstallationComplete
  deriving stock (Eq, Ord, Show, Generic)

data InstallProgress = InstallProgress
  { installStage :: InstallStage
  , installBytesComplete :: Int64
  , installBytesTotal :: Maybe Int64
  , installMessage :: Text
  }
  deriving stock (Eq, Show, Generic)

data Settings = Settings
  { settingsVersion :: Int
  , audioDeviceName :: Maybe Text
  , voiceThresholdDb :: Float
  , trailingSilenceMs :: Int
  , maximumUtteranceSeconds :: Int
  , inferenceThreads :: Int
  , keepAudio :: Bool
  , selectedModelId :: Text
  , activationHotkey :: HotkeyPreset
  , launchMinimized :: Bool
  , playAudioCues :: Bool
  , pasteAfterDictation :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o ->
    Settings
      <$> o .:? "settingsVersion" .!= 1
      <*> o .:? "audioDeviceName"
      <*> o .:? "voiceThresholdDb" .!= (-42)
      <*> o .:? "trailingSilenceMs" .!= 700
      <*> o .:? "maximumUtteranceSeconds" .!= 30
      <*> o .:? "inferenceThreads" .!= 2
      <*> o .:? "keepAudio" .!= False
      <*> o .:? "selectedModelId" .!= "parakeet-tdt-0.6b-v3-int8"
      <*> o .:? "activationHotkey" .!= ControlShiftSpace
      <*> o .:? "launchMinimized" .!= False
      <*> o .:? "playAudioCues" .!= True
      <*> o .:? "pasteAfterDictation" .!= True

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsVersion = 2
    , audioDeviceName = Nothing
    , voiceThresholdDb = -42
    , trailingSilenceMs = 700
    , maximumUtteranceSeconds = 30
    , inferenceThreads = 2
    , keepAudio = False
    , selectedModelId = "parakeet-tdt-0.6b-v3-int8"
    , activationHotkey = ControlShiftSpace
    , launchMinimized = False
    , playAudioCues = True
    , pasteAfterDictation = True
    }

data HotkeyPreset
  = ControlShiftSpace
  | ControlAltSpace
  | SuperShiftSpace
  | FunctionKey8
  | FunctionKey9
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ActivationSource
  = HotkeyActivation
  | WindowActivation
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RecognitionResult = RecognitionResult
  { recognizedText :: Text
  , recognizedLanguage :: Maybe Text
  , tokenTimestamps :: [Float]
  , tokenDurations :: [Float]
  , recognizedTokens :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON RecognitionResult where
  parseJSON = withObject "RecognitionResult" $ \o ->
    RecognitionResult
      <$> o .:? "text" .!= ""
      <*> o .:? "lang"
      <*> o .:? "timestamps" .!= []
      <*> o .:? "durations" .!= []
      <*> o .:? "tokens" .!= []

instance ToJSON RecognitionResult where
  toJSON result =
    object
      [ "text" .= recognizedText result
      , "lang" .= recognizedLanguage result
      , "timestamps" .= tokenTimestamps result
      , "durations" .= tokenDurations result
      , "tokens" .= recognizedTokens result
      ]

emptyRecognitionResult :: RecognitionResult
emptyRecognitionResult = RecognitionResult "" Nothing [] [] []

data TranscriptSegment = TranscriptSegment
  { segmentNumber :: Int
  , segmentText :: Text
  , segmentLanguage :: Maybe Text
  , segmentAudioSeconds :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TranscriptRecord = TranscriptRecord
  { recordId :: Text
  , recordStartedAt :: Text
  , recordCompletedAt :: Text
  , recordSource :: ActivationSource
  , recordModelId :: Text
  , recordText :: Text
  , recordLanguage :: Maybe Text
  , recordAudioSeconds :: Float
  , recordInjected :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

renderTranscript :: [TranscriptSegment] -> Text
renderTranscript = Text.intercalate "\n\n" . fmap segmentText
