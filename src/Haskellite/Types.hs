{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Types
  ( AppPaths (..)
  , ActivationSource (..)
  , HotkeyKey (..)
  , HotkeyModifiers (..)
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
  , hotkeyBinding
  , validHotkeyBinding
  , renderTranscript
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (String)
  , object
  , withObject
  , (.:)
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
  , holdHotkeyToTalk :: Bool
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
      <*> o .:? "holdHotkeyToTalk" .!= False
      <*> o .:? "launchMinimized" .!= False
      <*> o .:? "playAudioCues" .!= True
      <*> o .:? "pasteAfterDictation" .!= True

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsVersion = 4
    , audioDeviceName = Nothing
    , voiceThresholdDb = -42
    , trailingSilenceMs = 700
    , maximumUtteranceSeconds = 30
    , inferenceThreads = 2
    , keepAudio = False
    , selectedModelId = "parakeet-tdt-0.6b-v3-int8"
    , activationHotkey = ControlShiftSpace
    , holdHotkeyToTalk = False
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
  | CustomHotkey HotkeyModifiers HotkeyKey
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON HotkeyPreset where
  toJSON preset = case preset of
    ControlShiftSpace -> String "ControlShiftSpace"
    ControlAltSpace -> String "ControlAltSpace"
    SuperShiftSpace -> String "SuperShiftSpace"
    FunctionKey8 -> String "FunctionKey8"
    FunctionKey9 -> String "FunctionKey9"
    CustomHotkey modifiers key ->
      object
        [ "tag" .= ("CustomHotkey" :: Text)
        , "modifiers" .= modifiers
        , "key" .= key
        ]

instance FromJSON HotkeyPreset where
  parseJSON (String name) = case name of
    "ControlShiftSpace" -> pure ControlShiftSpace
    "ControlAltSpace" -> pure ControlAltSpace
    "SuperShiftSpace" -> pure SuperShiftSpace
    "FunctionKey8" -> pure FunctionKey8
    "FunctionKey9" -> pure FunctionKey9
    _ -> fail $ "Unknown shortcut preset: " <> Text.unpack name
  parseJSON value = withObject "HotkeyPreset" parseCustom value
    where
      parseCustom o = do
        tag <- o .: "tag"
        if (tag :: Text) == "CustomHotkey"
          then CustomHotkey <$> o .: "modifiers" <*> o .: "key"
          else fail $ "Unknown shortcut type: " <> Text.unpack tag

data HotkeyModifiers = HotkeyModifiers
  { modifierControl :: Bool
  , modifierShift :: Bool
  , modifierAlt :: Bool
  , modifierSuper :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HotkeyKey
  = HotkeyA
  | HotkeyB
  | HotkeyC
  | HotkeyD
  | HotkeyE
  | HotkeyF
  | HotkeyG
  | HotkeyH
  | HotkeyI
  | HotkeyJ
  | HotkeyK
  | HotkeyL
  | HotkeyM
  | HotkeyN
  | HotkeyO
  | HotkeyP
  | HotkeyQ
  | HotkeyR
  | HotkeyS
  | HotkeyT
  | HotkeyU
  | HotkeyV
  | HotkeyW
  | HotkeyX
  | HotkeyY
  | HotkeyZ
  | Hotkey0
  | Hotkey1
  | Hotkey2
  | Hotkey3
  | Hotkey4
  | Hotkey5
  | Hotkey6
  | Hotkey7
  | Hotkey8
  | Hotkey9
  | HotkeySpace
  | HotkeyTab
  | HotkeyReturn
  | HotkeyEscape
  | HotkeyBackspace
  | HotkeyLeft
  | HotkeyRight
  | HotkeyUp
  | HotkeyDown
  | HotkeyHome
  | HotkeyEnd
  | HotkeyPageUp
  | HotkeyPageDown
  | HotkeyInsert
  | HotkeyDelete
  | HotkeyMinus
  | HotkeyEquals
  | HotkeyLeftBracket
  | HotkeyRightBracket
  | HotkeyBackslash
  | HotkeySemicolon
  | HotkeyQuote
  | HotkeyBackquote
  | HotkeyComma
  | HotkeyPeriod
  | HotkeySlash
  | HotkeyF1
  | HotkeyF2
  | HotkeyF3
  | HotkeyF4
  | HotkeyF5
  | HotkeyF6
  | HotkeyF7
  | HotkeyF8
  | HotkeyF9
  | HotkeyF10
  | HotkeyF11
  | HotkeyF12
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

hotkeyBinding :: HotkeyPreset -> (HotkeyModifiers, HotkeyKey)
hotkeyBinding preset = case preset of
  ControlShiftSpace -> (HotkeyModifiers True True False False, HotkeySpace)
  ControlAltSpace -> (HotkeyModifiers True False True False, HotkeySpace)
  SuperShiftSpace -> (HotkeyModifiers False True False True, HotkeySpace)
  FunctionKey8 -> (HotkeyModifiers False False False False, HotkeyF8)
  FunctionKey9 -> (HotkeyModifiers False False False False, HotkeyF9)
  CustomHotkey modifiers key -> (modifiers, key)

validHotkeyBinding :: HotkeyModifiers -> HotkeyKey -> Bool
validHotkeyBinding modifiers key =
  modifierControl modifiers
    || modifierShift modifiers
    || modifierAlt modifiers
    || modifierSuper modifiers
    || key >= HotkeyF1

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
