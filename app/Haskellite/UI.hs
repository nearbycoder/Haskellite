{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Haskellite.UI
  ( runDesktop
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
  ( TQueue
  , atomically
  , newTQueueIO
  , tryReadTQueue
  , writeTQueue
  )
import Control.Exception (SomeException, bracket, bracket_, displayException, finally, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed, managed_, runManaged)
import Data.Bits (zeroBits)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import DearImGui
  ( ImVec2 (..)
  , ImVec4 (..)
  , beginDisabled
  , button
  , combo
  , createContext
  , destroyContext
  , endDisabled
  , getDrawData
  , getContentRegionAvail
  , inputTextMultiline
  , newFrame
  , progressBar
  , render
  , sameLine
  , separator
  , separatorText
  , sliderFloat
  , sliderInt
  , spacing
  , styleColorsDark
  , text
  , textColored
  , textWrapped
  , withChildOpen
  , withFullscreen
  )
import DearImGui.FontAtlas qualified as FontAtlas
import DearImGui.SDL (pollEventWithImGui, sdl2NewFrame, sdl2Shutdown)
import DearImGui.SDL.Renderer
  ( sdl2InitForSDLRenderer
  , sdlRendererInit
  , sdlRendererNewFrame
  , sdlRendererRenderDrawData
  , sdlRendererShutdown
  )
import Haskellite.Audio (listCaptureDevices)
import Haskellite.Controller
  ( Session
  , SessionEvent (..)
  , startSession
  , stopSession
  )
import Haskellite.Parakeet (Parakeet, closeParakeet, openParakeet)
import Haskellite.Runtime
  ( installParakeet
  , loadSettings
  , resolveModelPaths
  , resolveRuntimeLibraries
  , saveSettings
  )
import Haskellite.Types
  ( AppPaths (..)
  , InstallProgress (..)
  , InstallStage (..)
  , Settings (..)
  , TranscriptSegment
  , segmentText
  )
import Paths_haskellite (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified SDL

data EngineState
  = EngineMissing
  | EngineInstalling InstallProgress
  | EngineLoading
  | EngineReady Parakeet
  | EngineBroken Text

data UiModel = UiModel
  { engineState :: EngineState
  , activeSession :: Maybe Session
  , inputLevel :: Float
  , speechActive :: Bool
  , transcriptionActive :: Bool
  , statusMessage :: Text
  , lastError :: Maybe Text
  , transcriptSegments :: [TranscriptSegment]
  , backgroundWorkers :: [Async ()]
  }

data UiEvent
  = InstallChanged InstallProgress
  | EngineLoaded Parakeet
  | BackgroundFailed Text
  | FromSession SessionEvent
  | SessionStopped
  | TranscriptSaved FilePath

data UiRefs = UiRefs
  { paths :: AppPaths
  , baseSettings :: Settings
  , modelRef :: IORef UiModel
  , transcriptRef :: IORef Text
  , thresholdRef :: IORef Float
  , silenceRef :: IORef Int
  , deviceRef :: IORef Int
  , devices :: [Text]
  , events :: TQueue UiEvent
  }

runDesktop :: AppPaths -> IO ()
runDesktop appPaths = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio, SDL.InitEvents]
  ( do
      refs <- createUiRefs appPaths
      beginInitialLoad refs
      (runManaged do
        window <- do
          let windowConfig =
                SDL.defaultWindow
                  { SDL.windowInitialSize = SDL.V2 1040 720
                  , SDL.windowResizable = True
                  , SDL.windowPosition = SDL.Centered
                  }
          managed $ bracket (SDL.createWindow "Haskellite" windowConfig) SDL.destroyWindow
        renderer <- managed $ bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer
        _ <- managed $ bracket createContext destroyContext
        managed_ $ bracket_ (sdl2InitForSDLRenderer window renderer) sdl2Shutdown
        managed_ $ bracket_ (sdlRendererInit renderer) sdlRendererShutdown
        liftIO $ do
          styleColorsDark
          fontPath <- getDataFileName "assets/NotoSans-Regular.ttf"
          _ <-
            FontAtlas.rebuild
              [ FontAtlas.FromTTF
                  fontPath
                  18
                  Nothing
                  ( FontAtlas.RangesBuilder $
                      FontAtlas.addRanges FontAtlas.Latin
                        <> FontAtlas.addRanges FontAtlas.Cyrillic
                        <> FontAtlas.addText "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρστυφχψω—…’“”"
                  )
              ]
          mainLoop refs renderer)
        `finally` cleanup refs
    )
    `finally` SDL.quit

createUiRefs :: AppPaths -> IO UiRefs
createUiRefs paths = do
  settings <- loadSettings paths
  availableDevices <- listCaptureDevices
  let selectedDevice =
        case audioDeviceName settings of
          Nothing -> 0
          Just selected -> maybe 0 (+ 1) (findIndex selected availableDevices)
      model =
        UiModel
          { engineState = EngineMissing
          , activeSession = Nothing
          , inputLevel = 0
          , speechActive = False
          , transcriptionActive = False
          , statusMessage = "Checking local model…"
          , lastError = Nothing
          , transcriptSegments = []
          , backgroundWorkers = []
          }
  UiRefs paths settings
    <$> newIORef model
    <*> newIORef ""
    <*> newIORef (voiceThresholdDb settings)
    <*> newIORef (trailingSilenceMs settings)
    <*> newIORef selectedDevice
    <*> pure availableDevices
    <*> newTQueueIO

beginInitialLoad :: UiRefs -> IO ()
beginInitialLoad refs = do
  runtime <- resolveRuntimeLibraries (paths refs)
  model <- resolveModelPaths (paths refs)
  case (runtime, model) of
    (Right runtimeFiles, Right modelFiles) -> do
      modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineLoading, statusMessage = "Loading Parakeet…"}
      spawnWorker refs do
        loaded <- try $ openParakeet runtimeFiles modelFiles (inferenceThreads $ baseSettings refs)
        emit refs $ either (BackgroundFailed . exceptionText) EngineLoaded loaded
    _ -> modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineMissing, statusMessage = "Parakeet needs to be installed"}

mainLoop :: UiRefs -> SDL.Renderer -> IO ()
mainLoop refs renderer = do
  quit <- processEvents
  unless quit do
    drainEvents refs
    sdlRendererNewFrame
    sdl2NewFrame
    newFrame
    renderApplication refs
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 12 17 25 255
    SDL.clear renderer
    render
    sdlRendererRenderDrawData renderer =<< getDrawData
    SDL.present renderer
    threadDelay 8000
    mainLoop refs renderer

processEvents :: IO Bool
processEvents =
  pollEventWithImGui >>= \case
    Nothing -> pure False
    Just event ->
      if SDL.eventPayload event == SDL.QuitEvent
        then pure True
        else processEvents

renderApplication :: UiRefs -> IO ()
renderApplication refs = withFullscreen do
  model <- readIORef (modelRef refs)
  coloredText (ImVec4 0.45 0.86 1 1) "HASKELLITE"
  sameLine
  coloredText (ImVec4 0.58 0.63 0.72 1) "Private, local voice transcription"
  separator
  spacing
  renderEnginePanel refs model
  spacing
  renderRecorder refs model
  spacing
  renderTranscript refs model
  spacing
  renderSettings refs model

renderEnginePanel :: UiRefs -> UiModel -> IO ()
renderEnginePanel refs model =
  case engineState model of
    EngineMissing -> withChildOpen "model-setup" (ImVec2 0 118) True zeroBits do
      coloredText (ImVec4 1 0.78 0.30 1) "Parakeet model setup"
      textWrapped "Download the multilingual NVIDIA Parakeet TDT 0.6B v3 INT8 model and its local inference runtime. The one-time download is about 473 MB. Audio never leaves this computer."
      whenM (button "Install Parakeet") $ startInstall refs
    EngineInstalling progress -> withChildOpen "model-progress" (ImVec2 0 112) True zeroBits do
      text $ stageLabel (installStage progress)
      textWrapped (installMessage progress)
      progressBar (progressFraction progress) (Just $ progressText progress)
    EngineLoading -> withChildOpen "model-loading" (ImVec2 0 78) True zeroBits do
      coloredText (ImVec4 0.45 0.86 1 1) "Loading Parakeet"
      textWrapped "Preparing the local recognition engine. This can take a few seconds."
    EngineBroken message -> withChildOpen "model-error" (ImVec2 0 118) True zeroBits do
      coloredText (ImVec4 1 0.38 0.42 1) "Parakeet could not start"
      textWrapped message
      whenM (button "Retry setup") $ startInstall refs
    EngineReady _ -> pure ()

renderRecorder :: UiRefs -> UiModel -> IO ()
renderRecorder refs model = withChildOpen "recorder" (ImVec2 0 136) True zeroBits do
  let isListening = isJust (activeSession model)
      engineAvailable = case engineState model of EngineReady _ -> True; _ -> False
      label = if isListening then "Stop & transcribe" else "Start listening"
  coloredText (statusColor model) (statusTitle model)
  textWrapped (statusMessage model)
  progressBar (inputLevel model) (Just $ if speechActive model then "Voice detected" else "Input level")
  beginDisabled (not engineAvailable && not isListening)
  clicked <- button label
  endDisabled
  when clicked $
    if isListening then requestStop refs model else requestStart refs model

renderTranscript :: UiRefs -> UiModel -> IO ()
renderTranscript refs model = do
  separatorText "Transcript"
  ImVec2 availableWidth _ <- getContentRegionAvail
  _ <- inputTextMultiline "##transcript" (transcriptRef refs) (1024 * 1024) (ImVec2 availableWidth 245)
  copyClicked <- button "Copy"
  when copyClicked $ SDL.setClipboardText =<< readIORef (transcriptRef refs)
  sameLine
  saveClicked <- button "Save .txt"
  when saveClicked $ saveTranscript refs
  sameLine
  clearClicked <- button "Clear"
  when clearClicked $ do
    writeIORef (transcriptRef refs) ""
    modifyIORef' (modelRef refs) $ \state -> state {transcriptSegments = [], lastError = Nothing, statusMessage = readyMessage state}
  when (transcriptionActive model) do
    sameLine
    coloredText (ImVec4 0.45 0.86 1 1) "Transcribing…"
  case lastError model of
    Nothing -> pure ()
    Just message -> coloredText (ImVec4 1 0.38 0.42 1) message

renderSettings :: UiRefs -> UiModel -> IO ()
renderSettings refs model = do
  separatorText "Input settings"
  beginDisabled (isJust $ activeSession model)
  changedDevice <- combo "Microphone" (deviceRef refs) ("System default" : devices refs)
  changedThreshold <- sliderFloat "Voice sensitivity (dB)" (thresholdRef refs) (-60) (-20)
  changedSilence <- sliderInt "End phrase after silence (ms)" (silenceRef refs) 300 1600
  endDisabled
  when (changedDevice || changedThreshold || changedSilence) $ saveCurrentSettings refs

startInstall :: UiRefs -> IO ()
startInstall refs = do
  let initial = InstallProgress CheckingFiles 0 Nothing "Checking files"
  modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineInstalling initial, statusMessage = "Installing Parakeet…", lastError = Nothing}
  spawnWorker refs do
    installed <- try $ installParakeet (paths refs) (emit refs . InstallChanged)
    case installed of
      Left exception -> emit refs (BackgroundFailed $ exceptionText (exception :: SomeException))
      Right () -> do
        runtime <- resolveRuntimeLibraries (paths refs)
        model <- resolveModelPaths (paths refs)
        case (runtime, model) of
          (Right runtimeFiles, Right modelFiles) -> do
            loaded <- try $ openParakeet runtimeFiles modelFiles (inferenceThreads $ baseSettings refs)
            emit refs $ either (BackgroundFailed . exceptionText) EngineLoaded loaded
          (Left message, _) -> emit refs (BackgroundFailed message)
          (_, Left message) -> emit refs (BackgroundFailed message)

requestStart :: UiRefs -> UiModel -> IO ()
requestStart refs model =
  case engineState model of
    EngineReady parakeet -> do
      settings <- currentSettings refs
      started <- try $ startSession parakeet settings (emit refs . FromSession)
      case started of
        Left exception -> modifyIORef' (modelRef refs) $ \state -> state {lastError = Just (exceptionText (exception :: SomeException)), statusMessage = "Microphone could not be opened"}
        Right session -> modifyIORef' (modelRef refs) $ \state -> state {activeSession = Just session, statusMessage = "Listening — speak naturally", lastError = Nothing}
    _ -> pure ()

requestStop :: UiRefs -> UiModel -> IO ()
requestStop refs model =
  case activeSession model of
    Nothing -> pure ()
    Just session -> do
      modifyIORef' (modelRef refs) $ \state -> state {statusMessage = "Finishing the last phrase…", speechActive = False}
      spawnWorker refs do
        stopped <- try $ stopSession session
        emit refs $ either (BackgroundFailed . exceptionText) (const SessionStopped) stopped

saveTranscript :: UiRefs -> IO ()
saveTranscript refs = do
  contents <- readIORef (transcriptRef refs)
  unless (Text.null $ Text.strip contents) $ spawnWorker refs do
    now <- getCurrentTime
    let directory = transcriptDirectory (paths refs)
        filename = "Haskellite-" <> formatTime defaultTimeLocale "%Y-%m-%d-%H%M%S" now <> ".txt"
        destination = directory </> filename
    createDirectoryIfMissing True directory
    TextIO.writeFile destination contents
    emit refs (TranscriptSaved destination)

drainEvents :: UiRefs -> IO ()
drainEvents refs = do
  next <- atomically $ tryReadTQueue (events refs)
  case next of
    Nothing -> pure ()
    Just event -> handleUiEvent refs event >> drainEvents refs

handleUiEvent :: UiRefs -> UiEvent -> IO ()
handleUiEvent refs = \case
  InstallChanged progress -> modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineInstalling progress}
  EngineLoaded parakeet -> modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineReady parakeet, statusMessage = "Ready — press Start listening", lastError = Nothing}
  BackgroundFailed message -> modifyIORef' (modelRef refs) $ \state -> state {engineState = engineAfterFailure state message, activeSession = Nothing, statusMessage = "Something went wrong", lastError = Just message}
  FromSession sessionEvent -> handleSessionEvent refs sessionEvent
  SessionStopped -> modifyIORef' (modelRef refs) $ \state -> state {activeSession = Nothing, inputLevel = 0, speechActive = False, transcriptionActive = False, statusMessage = "Ready — press Start listening"}
  TranscriptSaved destination -> modifyIORef' (modelRef refs) $ \state -> state {statusMessage = "Saved to " <> Text.pack destination}

handleSessionEvent :: UiRefs -> SessionEvent -> IO ()
handleSessionEvent refs = \case
  InputLevel level -> modifyIORef' (modelRef refs) $ \state -> state {inputLevel = level}
  SpeechBegan -> modifyIORef' (modelRef refs) $ \state -> state {speechActive = True, statusMessage = "Voice detected — keep speaking"}
  SpeechEnded -> modifyIORef' (modelRef refs) $ \state -> state {speechActive = False, statusMessage = "Listening for the next phrase…"}
  TranscriptionBegan -> modifyIORef' (modelRef refs) $ \state -> state {transcriptionActive = True}
  SessionFailed message -> modifyIORef' (modelRef refs) $ \state -> state {transcriptionActive = False, lastError = Just message}
  TranscriptionCompleted segment -> do
    current <- readIORef (transcriptRef refs)
    let separatorTextValue = if Text.null (Text.strip current) then "" else "\n\n"
        updated = current <> separatorTextValue <> segmentText segment
    writeIORef (transcriptRef refs) updated
    modifyIORef' (modelRef refs) $ \state -> state {transcriptionActive = False, transcriptSegments = transcriptSegments state <> [segment], statusMessage = if isJust (activeSession state) then "Listening for the next phrase…" else "Transcription complete"}

currentSettings :: UiRefs -> IO Settings
currentSettings refs = do
  threshold <- readIORef (thresholdRef refs)
  silence <- readIORef (silenceRef refs)
  selectedDevice <- readIORef (deviceRef refs)
  let settings = baseSettings refs
      device = if selectedDevice <= 0 then Nothing else Just (devices refs !! (selectedDevice - 1))
  pure settings {audioDeviceName = device, voiceThresholdDb = threshold, trailingSilenceMs = silence}

saveCurrentSettings :: UiRefs -> IO ()
saveCurrentSettings refs = currentSettings refs >>= saveSettings (paths refs)

cleanup :: UiRefs -> IO ()
cleanup refs = do
  void $ try @SomeException (saveCurrentSettings refs)
  model <- readIORef (modelRef refs)
  case activeSession model of
    Nothing -> pure ()
    Just session -> void $ try @SomeException (stopSession session)
  mapM_ cancel (backgroundWorkers model)
  mapM_ waitCatch (backgroundWorkers model)
  drainEvents refs
  finalModel <- readIORef (modelRef refs)
  case engineState finalModel of
    EngineReady parakeet -> closeParakeet parakeet
    _ -> pure ()

spawnWorker :: UiRefs -> IO () -> IO ()
spawnWorker refs work = do
  worker <- async work
  modifyIORef' (modelRef refs) $ \state -> state {backgroundWorkers = worker : backgroundWorkers state}

emit :: UiRefs -> UiEvent -> IO ()
emit refs = atomically . writeTQueue (events refs)

findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex needle = go 0
  where
    go _ [] = Nothing
    go index (item : rest)
      | needle == item = Just index
      | otherwise = go (index + 1) rest

engineAfterFailure :: UiModel -> Text -> EngineState
engineAfterFailure model message =
  case engineState model of
    EngineInstalling _ -> EngineBroken message
    EngineLoading -> EngineBroken message
    ready@(EngineReady _) -> ready
    other -> other

statusTitle :: UiModel -> Text
statusTitle model
  | isJust (activeSession model) && speechActive model = "Recording speech"
  | isJust (activeSession model) = "Listening"
  | transcriptionActive model = "Transcribing"
  | otherwise = "Ready"

statusColor :: UiModel -> ImVec4
statusColor model
  | speechActive model = ImVec4 1 0.38 0.42 1
  | isJust (activeSession model) = ImVec4 0.30 0.92 0.67 1
  | otherwise = ImVec4 0.45 0.86 1 1

readyMessage :: UiModel -> Text
readyMessage state = case engineState state of EngineReady _ -> "Ready — press Start listening"; _ -> statusMessage state

stageLabel :: InstallStage -> Text
stageLabel = \case
  CheckingFiles -> "Checking files"
  DownloadingRuntime -> "Downloading local runtime"
  VerifyingRuntime -> "Verifying runtime"
  ExtractingRuntime -> "Extracting runtime"
  DownloadingModel -> "Downloading NVIDIA Parakeet"
  VerifyingModel -> "Verifying model"
  ExtractingModel -> "Extracting model"
  InstallationComplete -> "Installation complete"

progressFraction :: InstallProgress -> Float
progressFraction InstallProgress {installBytesComplete, installBytesTotal} =
  case installBytesTotal of
    Just total | total > 0 -> min 1 (fromIntegral installBytesComplete / fromIntegral total)
    _ -> 0

progressText :: InstallProgress -> Text
progressText InstallProgress {installBytesComplete, installBytesTotal} =
  case installBytesTotal of
    Nothing -> "Working…"
    Just total -> Text.pack $ printf "%.1f / %.1f MB" (toMb installBytesComplete) (toMb total)
  where
    toMb bytes = fromIntegral bytes / (1024 * 1024 :: Double)

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = condition >>= (`when` action)

coloredText :: ImVec4 -> Text -> IO ()
coloredText color message = do
  colorRef <- newIORef color
  textColored colorRef message
