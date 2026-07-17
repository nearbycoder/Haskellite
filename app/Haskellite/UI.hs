{-# LANGUAGE CPP #-}
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
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (FormatTime, defaultTimeLocale, formatTime, getCurrentTime)
import DearImGui
  ( ImVec2 (..)
  , ImVec4 (..)
  , beginDisabled
  , button
  , checkbox
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
import Haskellite.Audio (AudioCue (..), listCaptureDevices, playAudioCue)
import Haskellite.Controller
  ( Session
  , SessionEvent (..)
  , startSession
  , stopSession
  )
import Haskellite.History (appendHistory, loadHistory)
import Haskellite.Parakeet (Parakeet, closeParakeet, openParakeet)
import Haskellite.Platform
  ( GlobalHotkey
  , PasteTarget
  , SystemTray
  , capturePasteTarget
  , hotkeyLabel
  , sendPasteShortcut
  , startGlobalHotkey
  , startSystemTray
  , stopGlobalHotkey
  , stopSystemTray
  )
import Haskellite.Runtime
  ( DownloadAsset (assetBytes)
  , ParakeetModel (..)
  , availableModels
  , installParakeetFor
  , loadSettings
  , resolveModelPathsFor
  , resolveRuntimeLibraries
  , saveSettings
  )
import Haskellite.Types
  ( ActivationSource (..)
  , AppPaths (..)
  , HotkeyPreset (..)
  , InstallProgress (..)
  , InstallStage (..)
  , Settings (..)
  , TranscriptRecord (..)
  , TranscriptSegment (..)
  )
import Haskellite.Types qualified as Types
import Paths_haskellite (getDataFileName)
import System.Directory (XdgDirectory (XdgData), copyFile, createDirectoryIfMissing, doesFileExist, getXdgDirectory)
#if defined(darwin_HOST_OS)
import System.Environment (getExecutablePath)
#endif
import System.FilePath ((</>), takeDirectory)
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
  , pendingActivation :: Maybe PendingActivation
  , historyRecords :: [TranscriptRecord]
  , compactOverlay :: Bool
  , hotkeyMessage :: Maybe Text
  , trayMessage :: Maybe Text
  , backgroundWorkers :: [Async ()]
  }

data PendingActivation = PendingActivation
  { pendingId :: Text
  , pendingStartedAt :: Text
  , pendingSource :: ActivationSource
  , pendingModelId :: Text
  , pendingPasteTarget :: Maybe PasteTarget
  , pendingSegments :: [TranscriptSegment]
  , pendingStopRequested :: Bool
  }

data UiEvent
  = InstallChanged InstallProgress
  | EngineLoaded Text Parakeet
  | BackgroundFailed Text
  | FromSession SessionEvent
  | SessionStopped
  | TranscriptSaved FilePath
  | GlobalHotkeyPressed
  | DictationArchived TranscriptRecord (Maybe Text)
  | TrayShowRequested
  | TrayQuitRequested

data UiRefs = UiRefs
  { paths :: AppPaths
  , baseSettings :: Settings
  , modelRef :: IORef UiModel
  , transcriptRef :: IORef Text
  , thresholdRef :: IORef Float
  , silenceRef :: IORef Int
  , deviceRef :: IORef Int
  , modelChoiceRef :: IORef Int
  , hotkeyChoiceRef :: IORef Int
  , pasteRef :: IORef Bool
  , cuesRef :: IORef Bool
  , launchMinimizedRef :: IORef Bool
  , windowRef :: IORef (Maybe SDL.Window)
  , globalHotkeyRef :: IORef (Maybe GlobalHotkey)
  , systemTrayRef :: IORef (Maybe SystemTray)
  , quitRef :: IORef Bool
  , pageRef :: IORef Int
  , devices :: [Text]
  , events :: TQueue UiEvent
  }

runDesktop :: AppPaths -> IO ()
runDesktop appPaths = do
  ensureDesktopIntegration
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
        liftIO $ do
          writeIORef (windowRef refs) (Just window)
          registerConfiguredHotkey refs
          registerSystemTray refs window
          when (launchMinimized $ baseSettings refs) $ SDL.hideWindow window
        renderer <- managed $ bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer
        _ <- managed $ bracket createContext destroyContext
        managed_ $ bracket_ (sdl2InitForSDLRenderer window renderer) sdl2Shutdown
        managed_ $ bracket_ (sdlRendererInit renderer) sdlRendererShutdown
        liftIO $ do
          styleColorsDark
          fontPath <- getAppDataFileName "assets/NotoSans-Regular.ttf"
          _ <-
            FontAtlas.rebuild
              [ FontAtlas.FromTTF
                  fontPath
                  18
                  Nothing
                  ( FontAtlas.RangesBuilder $
                      FontAtlas.addRanges FontAtlas.Latin
                        <> FontAtlas.addRanges FontAtlas.Cyrillic
                        <> FontAtlas.addText "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρστυφχψω—…’“”●"
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
  historyResult <- loadHistory paths
  let selectedDevice =
        case audioDeviceName settings of
          Nothing -> 0
          Just selected -> maybe 0 (+ 1) (findIndex selected availableDevices)
      selectedModel = fromMaybe 0 (findIndex (selectedModelId settings) $ fmap parakeetModelId availableModels)
      (history, historyError) = either (\message -> ([], Just message)) (\records -> (records, Nothing)) historyResult
      model =
        UiModel
          { engineState = EngineMissing
          , activeSession = Nothing
          , inputLevel = 0
          , speechActive = False
          , transcriptionActive = False
          , statusMessage = "Checking local model…"
          , lastError = historyError
          , transcriptSegments = []
          , pendingActivation = Nothing
          , historyRecords = history
          , compactOverlay = False
          , hotkeyMessage = Nothing
          , trayMessage = Nothing
          , backgroundWorkers = []
          }
  UiRefs
    <$> pure paths
    <*> pure settings
    <*> newIORef model
    <*> newIORef ""
    <*> newIORef (voiceThresholdDb settings)
    <*> newIORef (trailingSilenceMs settings)
    <*> newIORef selectedDevice
    <*> newIORef selectedModel
    <*> newIORef (fromMaybe 0 $ findIndex (activationHotkey settings) allHotkeys)
    <*> newIORef (pasteAfterDictation settings)
    <*> newIORef (playAudioCues settings)
    <*> newIORef (launchMinimized settings)
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef False
    <*> newIORef 0
    <*> pure availableDevices
    <*> newTQueueIO

beginInitialLoad :: UiRefs -> IO ()
beginInitialLoad refs = do
  identifier <- selectedModelIdentifier refs
  runtime <- resolveRuntimeLibraries (paths refs)
  model <- resolveModelPathsFor (paths refs) identifier
  case (runtime, model) of
    (Right runtimeFiles, Right modelFiles) -> do
      modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineLoading, statusMessage = "Loading Parakeet…"}
      spawnWorker refs do
        loaded <- try $ openParakeet runtimeFiles modelFiles (inferenceThreads $ baseSettings refs)
        emit refs $ either (BackgroundFailed . exceptionText) (EngineLoaded identifier) loaded
    _ -> modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineMissing, statusMessage = "Parakeet needs to be installed"}

mainLoop :: UiRefs -> SDL.Renderer -> IO ()
mainLoop refs renderer = do
  quit <- processEvents refs
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

processEvents :: UiRefs -> IO Bool
processEvents refs = do
  quitRequested <- readIORef (quitRef refs)
  if quitRequested
    then pure True
    else
      pollEventWithImGui >>= \case
        Nothing -> pure False
        Just event ->
          if SDL.eventPayload event == SDL.QuitEvent
            then hideApplication refs >> processEvents refs
            else processEvents refs

renderApplication :: UiRefs -> IO ()
renderApplication refs = withFullscreen do
  model <- readIORef (modelRef refs)
  if compactOverlay model
    then renderCompactOverlay refs model
    else do
      coloredText (ImVec4 0.45 0.86 1 1) "HASKELLITE"
      sameLine
      coloredText (ImVec4 0.58 0.63 0.72 1) "Private, local voice transcription"
      separator
      spacing
      renderNavigation refs
      separator
      spacing
      page <- readIORef (pageRef refs)
      case page of
        1 -> renderHistory refs model
        2 -> renderSettingsPage refs model
        _ -> do
          renderEnginePanel refs model
          spacing
          renderRecorder refs model
          spacing
          renderTranscript refs model

renderNavigation :: UiRefs -> IO ()
renderNavigation refs = do
  current <- readIORef (pageRef refs)
  dictationClicked <- button $ if current == 0 then "● Dictate" else "Dictate"
  sameLine
  historyClicked <- button $ if current == 1 then "● History" else "History"
  sameLine
  settingsClicked <- button $ if current == 2 then "● Settings" else "Settings"
  when dictationClicked $ writeIORef (pageRef refs) 0
  when historyClicked $ writeIORef (pageRef refs) 1
  when settingsClicked $ writeIORef (pageRef refs) 2

renderSettingsPage :: UiRefs -> UiModel -> IO ()
renderSettingsPage refs model = do
  renderEnginePanel refs model
  spacing
  renderSettings refs model

renderCompactOverlay :: UiRefs -> UiModel -> IO ()
renderCompactOverlay refs model = do
  let listening = isJust (activeSession model)
      stopping = maybe False pendingStopRequested (pendingActivation model)
  if listening
    then do
      coloredText (statusColor model) $
        if stopping
          then "● Finishing…"
          else if speechActive model then "● Recording" else "● Listening"
      sameLine
      beginDisabled stopping
      stopClicked <- button $ if stopping then "Finishing…" else "Finish"
      endDisabled
      progressBar (inputLevel model) (Just "")
      when stopClicked $ requestStop refs model
    else do
      coloredText (statusColor model) "Recording unavailable"
      closeClicked <- button "Close"
      when closeClicked $ hideApplication refs
      sameLine
      openClicked <- button "Open Haskellite"
      when openClicked $ showMainApplication refs

renderEnginePanel :: UiRefs -> UiModel -> IO ()
renderEnginePanel refs model = do
  selected <- selectedModelDefinition refs
  case engineState model of
    EngineMissing -> withChildOpen "model-setup" (ImVec2 0 118) True zeroBits do
      coloredText (ImVec4 1 0.78 0.30 1) (parakeetModelName selected <> " setup")
      textWrapped $ parakeetModelSummary selected <> ". One-time model download: " <> downloadSizeLabel selected <> ". Audio never leaves this computer."
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
  unless isListening do
    shortcut <- selectedHotkey refs
    coloredText (ImVec4 0.58 0.70 0.82 1) $ "Global shortcut: " <> hotkeyLabel shortcut
  progressBar (inputLevel model) (Just $ if speechActive model then "Voice detected" else "Input level")
  beginDisabled (not engineAvailable && not isListening)
  clicked <- button label
  endDisabled
  when clicked $
    if isListening then requestStop refs model else requestStart refs model WindowActivation Nothing

renderTranscript :: UiRefs -> UiModel -> IO ()
renderTranscript refs model = do
  separatorText "Transcript"
  ImVec2 availableWidth _ <- getContentRegionAvail
  _ <- inputTextMultiline "##transcript" (transcriptRef refs) (1024 * 1024) (ImVec2 availableWidth 295)
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

renderHistory :: UiRefs -> UiModel -> IO ()
renderHistory _ model = do
  separatorText "Recent dictations"
  if null (historyRecords model)
    then textWrapped "Your completed dictations will appear here automatically."
    else withChildOpen "dictation-history" (ImVec2 0 490) True zeroBits $
      mapM_ renderRecord (take 20 . reverse $ historyRecords model)
  where
    renderRecord record = do
      coloredText (ImVec4 0.58 0.70 0.82 1) $ historyTimeLabel record <> "  ·  " <> modelNameForId (recordModelId record)
      let contents = Text.strip (recordText record)
      textWrapped $ if Text.null contents then "No speech detected" else contents
      unless (Text.null contents) do
        copied <- button $ "Copy##history-" <> recordId record
        when copied $ SDL.setClipboardText contents
      separator

renderSettings :: UiRefs -> UiModel -> IO ()
renderSettings refs model = do
  separatorText "Settings"
  hotkey <- selectedHotkey refs
  beginDisabled (isJust (activeSession model) || engineBusy (engineState model))
  changedModel <- combo "Parakeet model" (modelChoiceRef refs) (fmap modelChoiceLabel availableModels)
  endDisabled
  beginDisabled (isJust $ activeSession model)
  changedHotkey <- combo "Global shortcut" (hotkeyChoiceRef refs) (fmap hotkeyLabel allHotkeys)
  changedDevice <- combo "Microphone" (deviceRef refs) ("System default" : devices refs)
  changedThreshold <- sliderFloat "Voice sensitivity (dB)" (thresholdRef refs) (-60) (-20)
  changedSilence <- sliderInt "End phrase after silence (ms)" (silenceRef refs) 300 1600
  changedPaste <- checkbox "Paste into the focused field after hotkey dictation" (pasteRef refs)
  changedCues <- checkbox "Play start and finish sounds" (cuesRef refs)
  changedLaunch <- checkbox "Launch in the background" (launchMinimizedRef refs)
  endDisabled
  when changedModel $ switchModel refs
  when changedHotkey $ registerConfiguredHotkey refs
  when (changedModel || changedHotkey || changedDevice || changedThreshold || changedSilence || changedPaste || changedCues || changedLaunch) $ saveCurrentSettings refs
  case hotkeyMessage model of
    Nothing -> coloredText (ImVec4 0.30 0.92 0.67 1) $ "Shortcut active: " <> hotkeyLabel hotkey
    Just message -> coloredText (ImVec4 1 0.60 0.32 1) message
  case trayMessage model of
    Nothing -> textWrapped "Closing the window keeps Haskellite available from the system tray."
    Just message -> coloredText (ImVec4 1 0.60 0.32 1) message
  exitClicked <- button "Exit Haskellite"
  when exitClicked $ writeIORef (quitRef refs) True

startInstall :: UiRefs -> IO ()
startInstall refs = do
  identifier <- selectedModelIdentifier refs
  let initial = InstallProgress CheckingFiles 0 Nothing "Checking files"
  modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineInstalling initial, statusMessage = "Installing Parakeet…", lastError = Nothing}
  spawnWorker refs do
    installed <- try $ installParakeetFor (paths refs) identifier (emit refs . InstallChanged)
    case installed of
      Left exception -> emit refs (BackgroundFailed $ exceptionText (exception :: SomeException))
      Right () -> do
        runtime <- resolveRuntimeLibraries (paths refs)
        model <- resolveModelPathsFor (paths refs) identifier
        case (runtime, model) of
          (Right runtimeFiles, Right modelFiles) -> do
            loaded <- try $ openParakeet runtimeFiles modelFiles (inferenceThreads $ baseSettings refs)
            emit refs $ either (BackgroundFailed . exceptionText) (EngineLoaded identifier) loaded
          (Left message, _) -> emit refs (BackgroundFailed message)
          (_, Left message) -> emit refs (BackgroundFailed message)

requestStart :: UiRefs -> UiModel -> ActivationSource -> Maybe PasteTarget -> IO ()
requestStart refs model source pasteTarget =
  case engineState model of
    EngineReady parakeet -> do
      settings <- currentSettings refs
      cuesEnabled <- readIORef (cuesRef refs)
      when cuesEnabled $ void (try @SomeException $ playAudioCue DictationStarted)
      now <- getCurrentTime
      started <- try $ startSession parakeet settings (emit refs . FromSession)
      case started of
        Left exception -> modifyIORef' (modelRef refs) $ \state -> state {lastError = Just (exceptionText (exception :: SomeException)), statusMessage = "Microphone could not be opened"}
        Right session ->
          modifyIORef' (modelRef refs) $ \state ->
            state
              { activeSession = Just session
              , pendingActivation =
                  Just
                    PendingActivation
                      { pendingId = Text.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%qZ" now
                      , pendingStartedAt = isoTimestamp now
                      , pendingSource = source
                      , pendingModelId = selectedModelId settings
                      , pendingPasteTarget = pasteTarget
                      , pendingSegments = []
                      , pendingStopRequested = False
                      }
              , statusMessage = "Listening — speak naturally"
              , lastError = Nothing
              }
    _ -> pure ()

requestStop :: UiRefs -> UiModel -> IO ()
requestStop refs model =
  case activeSession model of
    Nothing -> pure ()
    Just session
      | maybe False pendingStopRequested (pendingActivation model) -> pure ()
      | otherwise -> do
          modifyIORef' (modelRef refs) $ \state -> state {statusMessage = "Finishing the last phrase…", speechActive = False}
          modifyIORef' (modelRef refs) $ \state -> state {pendingActivation = fmap (\pending -> pending {pendingStopRequested = True}) (pendingActivation state)}
          spawnWorker refs do
            stopped <- try $ stopSession session
            emit refs $ either (BackgroundFailed . exceptionText) (const SessionStopped) stopped

handleGlobalHotkey :: UiRefs -> IO ()
handleGlobalHotkey refs = do
  model <- readIORef (modelRef refs)
  case activeSession model of
    Just _ -> requestStop refs model
    Nothing -> case engineState model of
      EngineReady _ -> do
        pasteTarget <- capturePasteTarget
        showCompactApplication refs
        requestStart refs model HotkeyActivation pasteTarget
      _ -> do
        showMainApplication refs
        modifyIORef' (modelRef refs) $ \state -> state {statusMessage = "Install or finish loading the selected Parakeet model first"}

registerConfiguredHotkey :: UiRefs -> IO ()
registerConfiguredHotkey refs = do
  previous <- readIORef (globalHotkeyRef refs)
  mapM_ stopGlobalHotkey previous
  preset <- selectedHotkey refs
  registered <- startGlobalHotkey preset (emit refs GlobalHotkeyPressed)
  case registered of
    Left message -> do
      writeIORef (globalHotkeyRef refs) Nothing
      modifyIORef' (modelRef refs) $ \state -> state {hotkeyMessage = Just $ "Global shortcut unavailable: " <> message}
    Right hotkey -> do
      writeIORef (globalHotkeyRef refs) (Just hotkey)
      modifyIORef' (modelRef refs) $ \state -> state {hotkeyMessage = Nothing}

registerSystemTray :: UiRefs -> SDL.Window -> IO ()
registerSystemTray refs window = do
  started <- startSystemTray window (emit refs TrayShowRequested) (emit refs TrayQuitRequested)
  case started of
    Left message ->
      modifyIORef' (modelRef refs) $ \state -> state {trayMessage = Just $ "System tray unavailable: " <> message <> ". The global shortcut will still work in the background."}
    Right tray -> do
      writeIORef (systemTrayRef refs) (Just tray)
      modifyIORef' (modelRef refs) $ \state -> state {trayMessage = Nothing}

showCompactApplication :: UiRefs -> IO ()
showCompactApplication refs = do
  modifyIORef' (modelRef refs) $ \state -> state {compactOverlay = True}
  withWindow refs $ \window -> do
    SDL.windowBordered window SDL.$= False
    SDL.windowSize window SDL.$= SDL.V2 300 76
    SDL.setWindowPosition window SDL.Centered
    SDL.showWindow window
    SDL.raiseWindow window

showMainApplication :: UiRefs -> IO ()
showMainApplication refs = do
  modifyIORef' (modelRef refs) $ \state -> state {compactOverlay = False}
  withWindow refs $ \window -> do
    SDL.windowBordered window SDL.$= True
    SDL.windowSize window SDL.$= SDL.V2 1040 720
    SDL.setWindowPosition window SDL.Centered
    SDL.showWindow window
    SDL.raiseWindow window

hideApplication :: UiRefs -> IO ()
hideApplication refs = withWindow refs SDL.hideWindow

withWindow :: UiRefs -> (SDL.Window -> IO ()) -> IO ()
withWindow refs action = readIORef (windowRef refs) >>= mapM_ action

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
  EngineLoaded identifier parakeet -> do
    selected <- selectedModelIdentifier refs
    if identifier == selected
      then modifyIORef' (modelRef refs) $ \state -> state {engineState = EngineReady parakeet, statusMessage = "Ready — press Start listening", lastError = Nothing}
      else closeParakeet parakeet
  BackgroundFailed message -> modifyIORef' (modelRef refs) $ \state -> state {engineState = engineAfterFailure state message, activeSession = Nothing, statusMessage = "Something went wrong", lastError = Just message}
  FromSession sessionEvent -> handleSessionEvent refs sessionEvent
  SessionStopped -> finalizeActivation refs
  TranscriptSaved destination -> modifyIORef' (modelRef refs) $ \state -> state {statusMessage = "Saved to " <> Text.pack destination}
  GlobalHotkeyPressed -> handleGlobalHotkey refs
  DictationArchived record failure ->
    modifyIORef' (modelRef refs) $ \state ->
      state
        { historyRecords = historyRecords state <> [record]
        , statusMessage = archiveStatus record failure
        , lastError = failure
        }
  TrayShowRequested -> showMainApplication refs
  TrayQuitRequested -> writeIORef (quitRef refs) True

handleSessionEvent :: UiRefs -> SessionEvent -> IO ()
handleSessionEvent refs = \case
  InputLevel level -> modifyIORef' (modelRef refs) $ \state -> state {inputLevel = level}
  SpeechBegan -> modifyIORef' (modelRef refs) $ \state -> state {speechActive = True, statusMessage = "Voice detected — keep speaking"}
  SpeechEnded -> do
    modifyIORef' (modelRef refs) $ \state -> state {speechActive = False, statusMessage = "Finishing your dictation…"}
    state <- readIORef (modelRef refs)
    case (activeSession state, pendingActivation state) of
      (Just _, Just pending)
        | pendingSource pending == HotkeyActivation && not (pendingStopRequested pending) -> do
            modifyIORef' (modelRef refs) $ \current -> current {pendingActivation = fmap (\active -> active {pendingStopRequested = True}) (pendingActivation current)}
            requestStop refs state
      _ -> pure ()
  TranscriptionBegan -> modifyIORef' (modelRef refs) $ \state -> state {transcriptionActive = True}
  SessionFailed message -> modifyIORef' (modelRef refs) $ \state -> state {transcriptionActive = False, lastError = Just message}
  TranscriptionCompleted segment -> do
    current <- readIORef (transcriptRef refs)
    let separatorTextValue = if Text.null (Text.strip current) then "" else "\n\n"
        updated = current <> separatorTextValue <> segmentText segment
    writeIORef (transcriptRef refs) updated
    modifyIORef' (modelRef refs) $ \state ->
      state
        { transcriptionActive = False
        , transcriptSegments = transcriptSegments state <> [segment]
        , pendingActivation = fmap (\pending -> pending {pendingSegments = pendingSegments pending <> [segment]}) (pendingActivation state)
        , statusMessage = if isJust (activeSession state) then "Listening for the next phrase…" else "Transcription complete"
        }

finalizeActivation :: UiRefs -> IO ()
finalizeActivation refs = do
  state <- readIORef (modelRef refs)
  case pendingActivation state of
    Nothing -> clearSession "Ready — press Start listening"
    Just pending -> do
      now <- getCurrentTime
      let segments = pendingSegments pending
          contents = Text.strip (Types.renderTranscript segments)
          language = listToMaybe [value | segment <- segments, Just value <- [segmentLanguage segment]]
          audioSeconds = sum (fmap segmentAudioSeconds segments)
          record =
            TranscriptRecord
              { recordId = pendingId pending
              , recordStartedAt = pendingStartedAt pending
              , recordCompletedAt = isoTimestamp now
              , recordSource = pendingSource pending
              , recordModelId = pendingModelId pending
              , recordText = contents
              , recordLanguage = language
              , recordAudioSeconds = audioSeconds
              , recordInjected = False
              }
          fromHotkey = pendingSource pending == HotkeyActivation
      pasteEnabled <- readIORef (pasteRef refs)
      cuesEnabled <- readIORef (cuesRef refs)
      when cuesEnabled $ void (try @SomeException $ playAudioCue DictationCompleted)
      clipboard <-
        if fromHotkey && pasteEnabled && not (Text.null contents)
          then try @SomeException $ SDL.setClipboardText contents
          else pure $ Right ()
      when fromHotkey $ hideApplication refs
      clearSession $ if Text.null contents then "No speech detected" else "Saving dictation…"
      spawnWorker refs do
        let shouldPaste = fromHotkey && pasteEnabled && not (Text.null contents)
        when shouldPaste $ threadDelay 180000
        pasted <-
          case clipboard of
            Left exception -> pure . Left $ "Could not copy the dictation: " <> exceptionText exception
            Right () -> if shouldPaste then sendPasteShortcut (pendingPasteTarget pending) else pure (Right ())
        let completedRecord = record {recordInjected = shouldPaste && either (const False) (const True) pasted}
        saved <- try @SomeException $ appendHistory (paths refs) completedRecord
        let failure = case (pasted, saved) of
              (Left pasteError, Left historyError) -> Just $ pasteError <> "; " <> exceptionText historyError
              (Left pasteError, Right ()) -> Just $ pasteError <> ". The text is still on the clipboard."
              (Right (), Left historyError) -> Just $ exceptionText historyError
              (Right (), Right ()) -> Nothing
        emit refs $ DictationArchived completedRecord failure
  where
    clearSession message =
      modifyIORef' (modelRef refs) $ \current ->
        current
          { activeSession = Nothing
          , inputLevel = 0
          , speechActive = False
          , transcriptionActive = False
          , pendingActivation = Nothing
          , statusMessage = message
          }

currentSettings :: UiRefs -> IO Settings
currentSettings refs = do
  threshold <- readIORef (thresholdRef refs)
  silence <- readIORef (silenceRef refs)
  selectedDevice <- readIORef (deviceRef refs)
  hotkey <- selectedHotkey refs
  pasteEnabled <- readIORef (pasteRef refs)
  cuesEnabled <- readIORef (cuesRef refs)
  startHidden <- readIORef (launchMinimizedRef refs)
  let settings = baseSettings refs
      device = if selectedDevice <= 0 then Nothing else Just (devices refs !! (selectedDevice - 1))
  identifier <- selectedModelIdentifier refs
  pure
    settings
      { audioDeviceName = device
      , voiceThresholdDb = threshold
      , trailingSilenceMs = silence
      , selectedModelId = identifier
      , activationHotkey = hotkey
      , pasteAfterDictation = pasteEnabled
      , playAudioCues = cuesEnabled
      , launchMinimized = startHidden
      }

saveCurrentSettings :: UiRefs -> IO ()
saveCurrentSettings refs = currentSettings refs >>= saveSettings (paths refs)

switchModel :: UiRefs -> IO ()
switchModel refs = do
  state <- readIORef (modelRef refs)
  case engineState state of
    EngineReady parakeet -> closeParakeet parakeet
    _ -> pure ()
  modifyIORef' (modelRef refs) $ \current ->
    current
      { engineState = EngineMissing
      , statusMessage = "Checking selected model…"
      , lastError = Nothing
      }
  beginInitialLoad refs

cleanup :: UiRefs -> IO ()
cleanup refs = do
  void $ try @SomeException (saveCurrentSettings refs)
  hotkey <- readIORef (globalHotkeyRef refs)
  mapM_ stopGlobalHotkey hotkey
  writeIORef (globalHotkeyRef refs) Nothing
  tray <- readIORef (systemTrayRef refs)
  mapM_ stopSystemTray tray
  writeIORef (systemTrayRef refs) Nothing
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

archiveStatus :: TranscriptRecord -> Maybe Text -> Text
archiveStatus record failure = case failure of
  Just _ -> "Dictation saved, but delivery needs attention"
  Nothing
    | Text.null (Text.strip $ recordText record) -> "No speech detected"
    | recordInjected record -> "Dictation pasted"
    | otherwise -> "Dictation saved to history"

allHotkeys :: [HotkeyPreset]
allHotkeys = [ControlShiftSpace, ControlAltSpace, SuperShiftSpace, FunctionKey8, FunctionKey9]

selectedHotkey :: UiRefs -> IO HotkeyPreset
selectedHotkey refs = do
  choice <- readIORef (hotkeyChoiceRef refs)
  pure $ fromMaybe ControlShiftSpace (listToMaybe $ drop choice allHotkeys)

engineBusy :: EngineState -> Bool
engineBusy = \case
  EngineInstalling _ -> True
  EngineLoading -> True
  _ -> False

selectedModelDefinition :: UiRefs -> IO ParakeetModel
selectedModelDefinition refs = do
  choice <- readIORef (modelChoiceRef refs)
  pure $ fromMaybe fallback (listToMaybe $ drop choice availableModels)
  where
    fallback = fromMaybe (error "Haskellite has no Parakeet models") (listToMaybe availableModels)

selectedModelIdentifier :: UiRefs -> IO Text
selectedModelIdentifier refs = parakeetModelId <$> selectedModelDefinition refs

modelChoiceLabel :: ParakeetModel -> Text
modelChoiceLabel model = parakeetModelName model <> "  ·  " <> parakeetModelLanguages model

modelNameForId :: Text -> Text
modelNameForId identifier =
  maybe identifier parakeetModelName $ find ((== identifier) . parakeetModelId) availableModels

downloadSizeLabel :: ParakeetModel -> Text
downloadSizeLabel model =
  Text.pack . printf "%.0f MB" $
    (fromIntegral (assetBytes $ parakeetModelAsset model) / (1024 * 1024) :: Double)

historyTimeLabel :: TranscriptRecord -> Text
historyTimeLabel record = Text.replace "T" " " . Text.take 16 $ recordStartedAt record

isoTimestamp :: (FormatTime t) => t -> Text
isoTimestamp = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

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

ensureDesktopIntegration :: IO ()
#if defined(mingw32_HOST_OS) || defined(darwin_HOST_OS)
ensureDesktopIntegration = pure ()
#else
ensureDesktopIntegration = do
  source <- getAppDataFileName "packaging/haskellite.desktop"
  destination <- getXdgDirectory XdgData ("applications" </> "haskellite.desktop")
  exists <- doesFileExist destination
  unless exists $ do
    createDirectoryIfMissing True (takeDirectory destination)
    copyFile source destination
#endif

getAppDataFileName :: FilePath -> IO FilePath
#if defined(darwin_HOST_OS)
getAppDataFileName relativePath = do
  executable <- getExecutablePath
  let bundledPath = takeDirectory (takeDirectory executable) </> "Resources" </> relativePath
  bundled <- doesFileExist bundledPath
  if bundled then pure bundledPath else getDataFileName relativePath
#else
getAppDataFileName = getDataFileName
#endif
