{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Haskellite.History
  ( appendHistory
  , historyFilePath
  , loadHistory
  ) where

import Control.Exception (IOException, catch)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Haskellite.Types (AppPaths (appDataDirectory), TranscriptRecord)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import System.IO (IOMode (AppendMode), withBinaryFile)

historyFilePath :: AppPaths -> FilePath
historyFilePath paths = appDataDirectory paths </> "history.jsonl"

loadHistory :: AppPaths -> IO (Either Text [TranscriptRecord])
loadHistory paths = do
  let path = historyFilePath paths
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      contents <- ByteString.readFile path
      pure . traverse decodeLine . filter (not . ByteString.null) $ ByteString8.lines contents
  where
    decodeLine bytes =
      case eitherDecodeStrict' bytes of
        Left message -> Left $ "Could not read dictation history: " <> Text.pack message
        Right record -> Right record

appendHistory :: AppPaths -> TranscriptRecord -> IO ()
appendHistory paths record = do
  let path = historyFilePath paths
  createDirectoryIfMissing True (takeDirectory path)
  withBinaryFile path AppendMode $ \handle -> do
    Lazy.hPut handle (encode record)
    ByteString.hPut handle "\n"
  `catch` rethrowHistory
  where
    rethrowHistory :: IOException -> IO ()
    rethrowHistory exception = ioError . userError $ "Could not save dictation history: " <> show exception
