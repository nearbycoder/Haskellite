{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.RuntimeSpec (runtimeTests) where

import Data.ByteString qualified as ByteString
import Data.Either (isRight)
import Data.List (nub)
import Data.Text qualified as Text
import Haskellite.Runtime
  ( DownloadAsset (..)
  , ParakeetModel (..)
  , availableModels
  , currentRuntimeAsset
  , modelAsset
  , resolveModelPaths
  , modelById
  , resolveRuntimeLibraries
  , sha256File
  )
import Haskellite.Types (AppPaths (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

runtimeTests :: TestTree
runtimeTests =
  testGroup
    "runtime and model discovery"
    [ testCase "the current platform has a pinned runtime" $ do
        assertBool "supported platform" (isRight currentRuntimeAsset)
        either (const $ pure ()) (\asset -> Text.length (assetSha256 asset) @?= 64) currentRuntimeAsset
    , testCase "the Parakeet archive is checksum pinned" $
        Text.length (assetSha256 modelAsset) @?= 64
    , testCase "every selectable model has a unique id and checksum" $ do
        let identifiers = fmap parakeetModelId availableModels
        length identifiers @?= 3
        length (nub identifiers) @?= length identifiers
        mapM_ (\model -> Text.length (assetSha256 $ parakeetModelAsset model) @?= 64) availableModels
        mapM_ (\model -> modelById (parakeetModelId model) @?= Just model) availableModels
    , testCase "SHA-256 is streamed correctly" $
        withSystemTempDirectory "haskellite-sha" $ \directory -> do
          let path = directory </> "abc"
          ByteString.writeFile path "abc"
          digest <- sha256File path
          digest @?= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    , testCase "nested runtime and model files are discovered" $
        withSystemTempDirectory "haskellite-discovery" $ \directory -> do
          let runtimeRoot = directory </> "runtime"
              modelRoot = directory </> "model"
              nestedRuntime = runtimeRoot </> "package" </> "lib"
              nestedModel = modelRoot </> "package"
              paths = AppPaths directory (directory </> "settings.json") runtimeRoot modelRoot (directory </> "transcripts")
          createDirectoryIfMissing True nestedRuntime
          createDirectoryIfMissing True nestedModel
          mapM_ (\name -> ByteString.writeFile (nestedModel </> name) "") ["encoder.int8.onnx", "decoder.int8.onnx", "joiner.int8.onnx", "tokens.txt"]
          ByteString.writeFile (nestedRuntime </> apiName) ""
          ByteString.writeFile (nestedRuntime </> onnxName) ""
          model <- resolveModelPaths paths
          runtime <- resolveRuntimeLibraries paths
          assertBool "model found" (isRight model)
          assertBool "runtime found" (isRight runtime)
    ]

apiName :: FilePath
onnxName :: FilePath
#if defined(mingw32_HOST_OS)
apiName = "sherpa-onnx-c-api.dll"
onnxName = "onnxruntime.dll"
#elif defined(darwin_HOST_OS)
apiName = "libsherpa-onnx-c-api.dylib"
onnxName = "libonnxruntime.dylib"
#else
apiName = "libsherpa-onnx-c-api.so"
onnxName = "libonnxruntime.so"
#endif
