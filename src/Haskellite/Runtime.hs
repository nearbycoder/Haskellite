{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Runtime
  ( DownloadAsset (..)
  , ParakeetModel (..)
  , RuntimeLibraries (..)
  , availableModels
  , currentRuntimeAsset
  , discoverAppPaths
  , installParakeet
  , installParakeetFor
  , loadSettings
  , modelAsset
  , modelById
  , modelDirectoryFor
  , modelVersion
  , resolveModelPaths
  , resolveModelPathsFor
  , resolveRuntimeLibraries
  , runtimeVersion
  , saveSettings
  , sha256File
  ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.BZip qualified as BZip
import Control.Exception (IOException, bracketOnError, catch, throwIO)
import Control.Monad (foldM, unless, when)
import Crypto.Hash (Context, Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Haskellite.Types
  ( AppPaths (..)
  , InstallProgress (..)
  , InstallStage (..)
  , ModelPaths (..)
  , Settings (settingsVersion)
  , defaultSettings
  )
import Network.HTTP.Client
  ( BodyReader
  , Manager
  , brRead
  , newManager
  , parseRequest
  , responseBody
  , withResponse
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
  ( XdgDirectory (XdgData)
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , getXdgDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  , renameFile
  )
import System.FilePath ((</>), takeDirectory)
import System.Info qualified as Info
import System.IO
  ( Handle
  , IOMode (ReadMode, WriteMode)
  , hClose
  , openBinaryFile
  )

runtimeVersion :: Text
runtimeVersion = "1.13.2"

modelVersion :: Text
modelVersion = "parakeet-tdt-0.6b-v3-int8"

data DownloadAsset = DownloadAsset
  { assetName :: FilePath
  , assetUrl :: String
  , assetBytes :: Int64
  , assetSha256 :: Text
  }
  deriving (Eq, Show)

data RuntimeLibraries = RuntimeLibraries
  { runtimeApiLibrary :: FilePath
  , runtimeDependencies :: [FilePath]
  }
  deriving (Eq, Show)

data ParakeetModel = ParakeetModel
  { parakeetModelId :: Text
  , parakeetModelName :: Text
  , parakeetModelSummary :: Text
  , parakeetModelLanguages :: Text
  , parakeetModelAsset :: DownloadAsset
  }
  deriving (Eq, Show)

availableModels :: [ParakeetModel]
availableModels =
  [ ParakeetModel
      { parakeetModelId = modelVersion
      , parakeetModelName = "Multilingual 600M"
      , parakeetModelSummary = "Best all-around accuracy; automatic language detection"
      , parakeetModelLanguages = "25 European languages"
      , parakeetModelAsset = modelAsset
      }
  , ParakeetModel
      { parakeetModelId = "parakeet-tdt-0.6b-v2-int8"
      , parakeetModelName = "English Quality 600M"
      , parakeetModelSummary = "High-accuracy English dictation"
      , parakeetModelLanguages = "English"
      , parakeetModelAsset =
          DownloadAsset
            { assetName = "sherpa-onnx-nemo-parakeet-tdt-0.6b-v2-int8.tar.bz2"
            , assetUrl = "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v2-int8.tar.bz2"
            , assetBytes = 482468385
            , assetSha256 = "157c157bc51155e03e37d2466522a3a737dd9c72bb25f36eb18912964161e1ad"
            }
      }
  , ParakeetModel
      { parakeetModelId = "parakeet-tdt-transducer-110m-en-int8"
      , parakeetModelName = "English Fast 110M"
      , parakeetModelSummary = "Smallest download and quickest CPU response"
      , parakeetModelLanguages = "English"
      , parakeetModelAsset =
          DownloadAsset
            { assetName = "sherpa-onnx-nemo-parakeet_tdt_transducer_110m-en-36000-int8.tar.bz2"
            , assetUrl = "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet_tdt_transducer_110m-en-36000-int8.tar.bz2"
            , assetBytes = 108035095
            , assetSha256 = "f628312e9fdf8686374cb01a69425c41732529d540860311f16f37cbc32cfe9b"
            }
      }
  ]

modelById :: Text -> Maybe ParakeetModel
modelById identifier = find ((== identifier) . parakeetModelId) availableModels

modelAsset :: DownloadAsset
modelAsset =
  DownloadAsset
    { assetName = "sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8.tar.bz2"
    , assetUrl = "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8.tar.bz2"
    , assetBytes = 487170055
    , assetSha256 = "5793d0fd397c5778d2cf2126994d58e9d56b1be7c04d13c7a15bb1b4eafb16bf"
    }

currentRuntimeAsset :: Either Text DownloadAsset
currentRuntimeAsset = runtimeAssetFor Info.os Info.arch

runtimeAssetFor :: String -> String -> Either Text DownloadAsset
runtimeAssetFor operatingSystem architecture =
  case (operatingSystem, architecture) of
    ("linux", "x86_64") -> asset "linux-x64-shared-no-tts-lib" 8522001 "1c66f4ec57cbf6a608f09e373796346943702251f75d08c45e8f47345a960ee6"
    ("linux", "aarch64") -> asset "linux-aarch64-shared-cpu-lib" 11688458 "44449a83f19649b2466c97b4d2df57c158dbede6e1e044f6519cf297df17585c"
    ("darwin", "x86_64") -> asset "osx-universal2-shared-no-tts-lib" 30607258 "117150cf014ed913b1f5aee75eccfafa7957919b6efe8b9a3974bbcb5f7d6020"
    ("darwin", "aarch64") -> asset "osx-universal2-shared-no-tts-lib" 30607258 "117150cf014ed913b1f5aee75eccfafa7957919b6efe8b9a3974bbcb5f7d6020"
    ("mingw32", "x86_64") -> asset "win-x64-shared-MD-Release-no-tts-lib" 6373014 "6ddd96bd875349b0580d0bbfd70fb08694ad1b7ef9f02966005aec5c7824b700"
    ("mingw32", "aarch64") -> asset "win-arm64-shared-MD-Release-no-tts-lib" 6022440 "eb48fe070539cf804fa53622cf9f73c774d88ab150ef6a94c9ac25ec0ce8d26d"
    _ -> Left $ "No sherpa-onnx runtime is published for " <> Text.pack operatingSystem <> "/" <> Text.pack architecture
  where
    asset suffix bytes digest =
      let name = "sherpa-onnx-v1.13.2-" <> suffix <> ".tar.bz2"
       in Right
            DownloadAsset
              { assetName = name
              , assetUrl = "https://github.com/k2-fsa/sherpa-onnx/releases/download/v1.13.2/" <> name
              , assetBytes = bytes
              , assetSha256 = digest
              }

discoverAppPaths :: IO AppPaths
discoverAppPaths = do
  dataDirectory <- getXdgDirectory XdgData "haskellite"
  home <- getHomeDirectory
  let transcripts = home </> "Documents" </> "Haskellite"
  pure
    AppPaths
      { appDataDirectory = dataDirectory
      , settingsFile = dataDirectory </> "settings.json"
      , runtimeDirectory = dataDirectory </> "runtime" </> Text.unpack runtimeVersion
      , modelDirectory = dataDirectory </> "models" </> Text.unpack modelVersion
      , transcriptDirectory = transcripts
      }

loadSettings :: AppPaths -> IO Settings
loadSettings paths = do
  exists <- doesFileExist (settingsFile paths)
  if not exists
    then pure defaultSettings
    else do
      decoded <- Aeson.eitherDecodeFileStrict' (settingsFile paths)
      pure $ either (const defaultSettings) (\settings -> settings {settingsVersion = settingsVersion defaultSettings}) decoded

saveSettings :: AppPaths -> Settings -> IO ()
saveSettings paths settings = do
  createDirectoryIfMissing True (takeDirectory $ settingsFile paths)
  Lazy.writeFile (settingsFile paths <> ".tmp") (Aeson.encode settings)
  replaceFile (settingsFile paths <> ".tmp") (settingsFile paths)

resolveModelPaths :: AppPaths -> IO (Either Text ModelPaths)
resolveModelPaths paths = resolveModelPathsFor paths modelVersion

modelDirectoryFor :: AppPaths -> Text -> FilePath
modelDirectoryFor paths identifier
  | identifier == modelVersion = modelDirectory paths
  | otherwise = takeDirectory (modelDirectory paths) </> Text.unpack identifier

resolveModelPathsFor :: AppPaths -> Text -> IO (Either Text ModelPaths)
resolveModelPathsFor paths identifier = do
  let directory = modelDirectoryFor paths identifier
  encoder <- findNamedFile directory ["encoder.int8.onnx"]
  decoder <- findNamedFile directory ["decoder.int8.onnx"]
  joiner <- findNamedFile directory ["joiner.int8.onnx"]
  tokens <- findNamedFile directory ["tokens.txt"]
  pure $ ModelPaths <$> required "encoder.int8.onnx" encoder <*> required "decoder.int8.onnx" decoder <*> required "joiner.int8.onnx" joiner <*> required "tokens.txt" tokens
  where
    required name = maybe (Left $ "Missing Parakeet file: " <> Text.pack name) Right

resolveRuntimeLibraries :: AppPaths -> IO (Either Text RuntimeLibraries)
resolveRuntimeLibraries paths = do
  api <- findNamedFile (runtimeDirectory paths) apiNames
  onnx <- findNamedFile (runtimeDirectory paths) onnxNames
  pure $ RuntimeLibraries <$> required "sherpa-onnx C API library" api <*> (pure . maybe [] pure) onnx
  where
#if defined(mingw32_HOST_OS)
    apiNames = ["sherpa-onnx-c-api.dll", "libsherpa-onnx-c-api.dll"]
    onnxNames = ["onnxruntime.dll"]
#elif defined(darwin_HOST_OS)
    apiNames = ["libsherpa-onnx-c-api.dylib"]
    onnxNames = ["libonnxruntime.dylib"]
#else
    apiNames = ["libsherpa-onnx-c-api.so"]
    onnxNames = ["libonnxruntime.so"]
#endif
    required description = maybe (Left $ "Missing " <> description) Right

installParakeet :: AppPaths -> (InstallProgress -> IO ()) -> IO ()
installParakeet paths = installParakeetFor paths modelVersion

installParakeetFor :: AppPaths -> Text -> (InstallProgress -> IO ()) -> IO ()
installParakeetFor paths identifier notify = do
  selectedModel <-
    maybe
      (throwIO . userError $ "Unknown Parakeet model: " <> Text.unpack identifier)
      pure
      (modelById identifier)
  notify $ progress CheckingFiles 0 Nothing "Checking local runtime and model files"
  runtimeReady <- either (const False) (const True) <$> resolveRuntimeLibraries paths
  modelReady <- either (const False) (const True) <$> resolveModelPathsFor paths identifier
  manager <- newManager tlsManagerSettings
  unless runtimeReady $ do
    runtimeAsset <- either (throwIO . userError . Text.unpack) pure currentRuntimeAsset
    installArchive manager paths runtimeAsset (runtimeDirectory paths) DownloadingRuntime VerifyingRuntime ExtractingRuntime notify
  unless modelReady $
    installArchive manager paths (parakeetModelAsset selectedModel) (modelDirectoryFor paths identifier) DownloadingModel VerifyingModel ExtractingModel notify
  finalRuntime <- resolveRuntimeLibraries paths
  finalModel <- resolveModelPathsFor paths identifier
  _ <- either (throwIO . userError . Text.unpack) pure finalRuntime
  _ <- either (throwIO . userError . Text.unpack) pure finalModel
  notify $ progress InstallationComplete 1 (Just 1) "Parakeet is ready"

installArchive :: Manager -> AppPaths -> DownloadAsset -> FilePath -> InstallStage -> InstallStage -> InstallStage -> (InstallProgress -> IO ()) -> IO ()
installArchive manager paths asset destination downloadStage verifyStage extractStage notify = do
  let downloads = appDataDirectory paths </> "downloads"
      archive = downloads </> assetName asset
  createDirectoryIfMissing True downloads
  notify $ progress downloadStage 0 (Just $ assetBytes asset) ("Downloading " <> Text.pack (assetName asset))
  download manager asset archive $ \complete ->
    notify $ progress downloadStage complete (Just $ assetBytes asset) ("Downloading " <> Text.pack (assetName asset))
  notify $ progress verifyStage 0 Nothing "Verifying SHA-256 checksum"
  digest <- sha256File archive
  unless (Text.toLower digest == Text.toLower (assetSha256 asset)) $ do
    removeFile archive `catch` ignoreIOException
    throwIO . userError $ "Checksum mismatch for " <> assetName asset
  notify $ progress extractStage 0 Nothing "Extracting files"
  removePathForcibly destination `catch` ignoreIOException
  createDirectoryIfMissing True destination
  extractTarBzip archive destination
  removeFile archive `catch` ignoreIOException

download :: Manager -> DownloadAsset -> FilePath -> (Int64 -> IO ()) -> IO ()
download manager asset destination report = do
  request <- parseRequest (assetUrl asset)
  let temporary = destination <> ".part"
  removeFile temporary `catch` ignoreIOException
  withResponse request manager $ \response ->
    bracketOnError
      (openBinaryFile temporary WriteMode)
      (\handle -> hClose handle >> removeFile temporary `catch` ignoreIOException)
      (\handle -> copyBody (responseBody response) handle 0 0)
  replaceFile temporary destination
  where
    copyBody :: BodyReader -> Handle -> Int64 -> Int64 -> IO ()
    copyBody reader handle complete lastReported = do
      chunk <- brRead reader
      if ByteString.null chunk
        then hClose handle >> report complete
        else do
          ByteString.hPut handle chunk
          let newComplete = complete + fromIntegral (ByteString.length chunk)
          newReported <-
            if newComplete - lastReported >= 1024 * 1024
              then report newComplete >> pure newComplete
              else pure lastReported
          copyBody reader handle newComplete newReported

sha256File :: FilePath -> IO Text
sha256File path = do
  handle <- openBinaryFile path ReadMode
  digest <- go handle (hashInit :: Context SHA256)
  hClose handle
  pure . Text.pack . show $ (digest :: Digest SHA256)
  where
    go handle context = do
      chunk <- ByteString.hGetSome handle (1024 * 1024)
      if ByteString.null chunk
        then pure (hashFinalize context)
        else go handle (hashUpdate context chunk)

extractTarBzip :: FilePath -> FilePath -> IO ()
extractTarBzip archive destination =
  Tar.unpack destination . Tar.read . BZip.decompress =<< Lazy.readFile archive

findNamedFile :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findNamedFile root names = do
  rootExists <- doesDirectoryExist root
  if not rootExists then pure Nothing else search root
  where
    search directory = do
      entries <- listDirectory directory
      let exact = find (`elem` names) entries
      case exact of
        Just name -> pure . Just $ directory </> name
        Nothing -> foldM searchChild Nothing entries
      where
        searchChild found _ | Just _ <- found = pure found
        searchChild Nothing entry = do
          let path = directory </> entry
          isDirectory <- doesDirectoryExist path
          if isDirectory then search path else pure Nothing

replaceFile :: FilePath -> FilePath -> IO ()
replaceFile source destination = do
  exists <- doesFileExist destination
  when exists $ removeFile destination
  renameFile source destination

progress :: InstallStage -> Int64 -> Maybe Int64 -> Text -> InstallProgress
progress installStage installBytesComplete installBytesTotal installMessage = InstallProgress {..}

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()
