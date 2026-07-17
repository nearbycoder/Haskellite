{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Internal.Sherpa
  ( SherpaApi
  , SherpaRecognizer
  , closeSherpaApi
  , createSherpaRecognizer
  , decodeSamples
  , destroySherpaRecognizer
  , loadSherpaApi
  ) where

#include <sherpa-onnx/c-api/c-api.h>

import Control.Exception (bracket, onException, throwIO)
import Control.Monad (forM, when)
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString qualified as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign
  ( FunPtr
  , Ptr
  , allocaBytesAligned
  , castPtr
  , nullPtr
  , plusPtr
  , pokeByteOff
  )
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Utils (fillBytes)
import Haskellite.Internal.DynamicLibrary
  ( DynamicLibrary
  , closeDynamicLibrary
  , loadDynamicLibrary
  , loadSymbol
  )
import Haskellite.Types (ModelPaths (..), RecognitionResult)

data COfflineRecognizer
data COfflineStream

type CreateRecognizerFn = Ptr () -> IO (Ptr COfflineRecognizer)
type DestroyRecognizerFn = Ptr COfflineRecognizer -> IO ()
type CreateStreamFn = Ptr COfflineRecognizer -> IO (Ptr COfflineStream)
type DestroyStreamFn = Ptr COfflineStream -> IO ()
type AcceptWaveformFn = Ptr COfflineStream -> CInt -> Ptr CFloat -> CInt -> IO ()
type DecodeStreamFn = Ptr COfflineRecognizer -> Ptr COfflineStream -> IO ()
type GetResultJsonFn = Ptr COfflineStream -> IO CString
type DestroyResultJsonFn = CString -> IO ()

foreign import ccall unsafe "dynamic"
  bindCreateRecognizer :: FunPtr CreateRecognizerFn -> CreateRecognizerFn

foreign import ccall unsafe "dynamic"
  bindDestroyRecognizer :: FunPtr DestroyRecognizerFn -> DestroyRecognizerFn

foreign import ccall unsafe "dynamic"
  bindCreateStream :: FunPtr CreateStreamFn -> CreateStreamFn

foreign import ccall unsafe "dynamic"
  bindDestroyStream :: FunPtr DestroyStreamFn -> DestroyStreamFn

foreign import ccall unsafe "dynamic"
  bindAcceptWaveform :: FunPtr AcceptWaveformFn -> AcceptWaveformFn

foreign import ccall safe "dynamic"
  bindDecodeStream :: FunPtr DecodeStreamFn -> DecodeStreamFn

foreign import ccall unsafe "dynamic"
  bindGetResultJson :: FunPtr GetResultJsonFn -> GetResultJsonFn

foreign import ccall unsafe "dynamic"
  bindDestroyResultJson :: FunPtr DestroyResultJsonFn -> DestroyResultJsonFn

data SherpaApi = SherpaApi
  { loadedLibraries :: [DynamicLibrary]
  , createRecognizerFn :: CreateRecognizerFn
  , destroyRecognizerFn :: DestroyRecognizerFn
  , createStreamFn :: CreateStreamFn
  , destroyStreamFn :: DestroyStreamFn
  , acceptWaveformFn :: AcceptWaveformFn
  , decodeStreamFn :: DecodeStreamFn
  , getResultJsonFn :: GetResultJsonFn
  , destroyResultJsonFn :: DestroyResultJsonFn
  }

newtype SherpaRecognizer = SherpaRecognizer (Ptr COfflineRecognizer)

loadSherpaApi :: [FilePath] -> FilePath -> IO SherpaApi
loadSherpaApi dependencyPaths apiPath = do
  dependencies <- forM dependencyPaths loadDynamicLibrary
  apiLibrary <- loadDynamicLibrary apiPath `onException` mapM_ closeDynamicLibrary (reverse dependencies)
  let libraries = dependencies <> [apiLibrary]
      symbol name = loadSymbol apiLibrary name `onException` mapM_ closeDynamicLibrary (reverse libraries)
  createRecognizerFn <- bindCreateRecognizer <$> symbol "SherpaOnnxCreateOfflineRecognizer"
  destroyRecognizerFn <- bindDestroyRecognizer <$> symbol "SherpaOnnxDestroyOfflineRecognizer"
  createStreamFn <- bindCreateStream <$> symbol "SherpaOnnxCreateOfflineStream"
  destroyStreamFn <- bindDestroyStream <$> symbol "SherpaOnnxDestroyOfflineStream"
  acceptWaveformFn <- bindAcceptWaveform <$> symbol "SherpaOnnxAcceptWaveformOffline"
  decodeStreamFn <- bindDecodeStream <$> symbol "SherpaOnnxDecodeOfflineStream"
  getResultJsonFn <- bindGetResultJson <$> symbol "SherpaOnnxGetOfflineStreamResultAsJson"
  destroyResultJsonFn <- bindDestroyResultJson <$> symbol "SherpaOnnxDestroyOfflineStreamResultJson"
  pure SherpaApi {loadedLibraries = libraries, ..}

closeSherpaApi :: SherpaApi -> IO ()
closeSherpaApi = mapM_ closeDynamicLibrary . reverse . loadedLibraries

createSherpaRecognizer :: SherpaApi -> ModelPaths -> Int -> IO SherpaRecognizer
createSherpaRecognizer api modelPaths threads =
  withRecognizerConfig modelPaths threads $ \configPointer -> do
    recognizer <- createRecognizerFn api configPointer
    when (recognizer == nullPtr) . throwIO . userError $
      "Parakeet could not be initialized. Check the model files and runtime architecture."
    pure (SherpaRecognizer recognizer)

destroySherpaRecognizer :: SherpaApi -> SherpaRecognizer -> IO ()
destroySherpaRecognizer api (SherpaRecognizer recognizer) = destroyRecognizerFn api recognizer

decodeSamples :: SherpaApi -> SherpaRecognizer -> Int -> Vector Float -> IO RecognitionResult
decodeSamples api (SherpaRecognizer recognizer) sampleRate samples =
  bracket acquire (destroyStreamFn api) $ \stream -> do
    Vector.unsafeWith samples $ \samplePointer ->
      acceptWaveformFn api stream (fromIntegral sampleRate) (castPtr samplePointer) (fromIntegral $ Vector.length samples)
    decodeStreamFn api recognizer stream
    resultJson <- bracket (acquireResult stream) (destroyResultJsonFn api) ByteString.packCString
    case eitherDecodeStrict' resultJson of
      Left message -> throwIO . userError $ "Could not decode Parakeet result: " <> message
      Right result -> pure result
  where
    acquire = do
      stream <- createStreamFn api recognizer
      when (stream == nullPtr) . throwIO . userError $ "Could not create a Parakeet recognition stream."
      pure stream
    acquireResult stream = do
      resultPointer <- getResultJsonFn api stream
      when (resultPointer == nullPtr) . throwIO . userError $ "Parakeet returned no recognition result."
      pure resultPointer

withRecognizerConfig :: ModelPaths -> Int -> (Ptr () -> IO a) -> IO a
withRecognizerConfig ModelPaths {encoderPath, decoderPath, joinerPath, tokensPath} threads action =
  withUtf8CString encoderPath $ \encoder ->
    withUtf8CString decoderPath $ \decoder ->
      withUtf8CString joinerPath $ \joiner ->
        withUtf8CString tokensPath $ \tokens ->
          withUtf8CString "cpu" $ \provider ->
            withUtf8CString "nemo_transducer" $ \modelType ->
              withUtf8CString "greedy_search" $ \decodingMethod ->
                allocaBytesAligned configSize configAlignment $ \config -> do
                  fillBytes config 0 configSize
                  let modelConfig = config `plusPtr` modelConfigOffset
                      transducerConfig = modelConfig `plusPtr` transducerOffset
                  pokeByteOff transducerConfig encoderOffset encoder
                  pokeByteOff transducerConfig decoderOffset decoder
                  pokeByteOff transducerConfig joinerOffset joiner
                  pokeByteOff modelConfig tokensOffset tokens
                  pokeByteOff modelConfig threadsOffset (fromIntegral (max 1 threads) :: CInt)
                  pokeByteOff modelConfig providerOffset provider
                  pokeByteOff modelConfig modelTypeOffset modelType
                  pokeByteOff config decodingMethodOffset decodingMethod
                  action (castPtr config)

withUtf8CString :: String -> (CString -> IO a) -> IO a
withUtf8CString value = ByteString.useAsCString (Text.encodeUtf8 $ Text.pack value)

configSize, configAlignment, modelConfigOffset, transducerOffset :: Int
configSize = #{size SherpaOnnxOfflineRecognizerConfig}
configAlignment = #{alignment SherpaOnnxOfflineRecognizerConfig}
modelConfigOffset = #{offset SherpaOnnxOfflineRecognizerConfig, model_config}
transducerOffset = #{offset SherpaOnnxOfflineModelConfig, transducer}

encoderOffset, decoderOffset, joinerOffset :: Int
encoderOffset = #{offset SherpaOnnxOfflineTransducerModelConfig, encoder}
decoderOffset = #{offset SherpaOnnxOfflineTransducerModelConfig, decoder}
joinerOffset = #{offset SherpaOnnxOfflineTransducerModelConfig, joiner}

tokensOffset, threadsOffset, providerOffset, modelTypeOffset, decodingMethodOffset :: Int
tokensOffset = #{offset SherpaOnnxOfflineModelConfig, tokens}
threadsOffset = #{offset SherpaOnnxOfflineModelConfig, num_threads}
providerOffset = #{offset SherpaOnnxOfflineModelConfig, provider}
modelTypeOffset = #{offset SherpaOnnxOfflineModelConfig, model_type}
decodingMethodOffset = #{offset SherpaOnnxOfflineRecognizerConfig, decoding_method}
