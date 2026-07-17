{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Haskellite.Wav
  ( WavAudio (..)
  , decodeWav
  , encodeWav16
  , readWavFile
  , writeWavFile
  ) where

import Control.Monad (unless, when)
import Data.Binary.Get
  ( Get
  , getByteString
  , getFloatle
  , getInt16le
  , getInt32le
  , getWord16le
  , getWord32le
  , isEmpty
  , runGetOrFail
  )
import Data.Binary.Put
  ( putByteString
  , putInt16le
  , putWord16le
  , putWord32le
  , runPut
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Int (Int16, Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector

data WavAudio = WavAudio
  { wavSampleRate :: Int
  , wavSamples :: Vector Float
  }
  deriving (Eq, Show)

data WaveFormat = WaveFormat
  { formatTag :: Int
  , formatChannels :: Int
  , formatSampleRate :: Int
  , formatBitsPerSample :: Int
  }

data WaveChunks = WaveChunks
  { waveFormat :: Maybe WaveFormat
  , waveData :: Maybe ByteString
  }

decodeWav :: ByteString -> Either Text WavAudio
decodeWav input =
  case runGetOrFail getWave (Lazy.fromStrict input) of
    Left (_, offset, message) -> Left $ "Invalid WAV at byte " <> Text.pack (show offset) <> ": " <> Text.pack message
    Right (_, _, result) -> result

readWavFile :: FilePath -> IO (Either Text WavAudio)
readWavFile path = decodeWav <$> ByteString.readFile path

encodeWav16 :: Int -> Vector Float -> ByteString
encodeWav16 sampleRate samples =
  Lazy.toStrict . runPut $ do
    let sampleCount = Vector.length samples
        dataBytes = sampleCount * 2
        riffBytes = 36 + dataBytes
    putByteString "RIFF"
    putWord32le (fromIntegral riffBytes)
    putByteString "WAVE"
    putByteString "fmt "
    putWord32le 16
    putWord16le 1
    putWord16le 1
    putWord32le (fromIntegral sampleRate)
    putWord32le (fromIntegral (sampleRate * 2))
    putWord16le 2
    putWord16le 16
    putByteString "data"
    putWord32le (fromIntegral dataBytes)
    Vector.mapM_ (putInt16le . floatToInt16) samples

writeWavFile :: FilePath -> Int -> Vector Float -> IO ()
writeWavFile path sampleRate = ByteString.writeFile path . encodeWav16 sampleRate

getWave :: Get (Either Text WavAudio)
getWave = do
  riff <- getByteString 4
  unless (riff == "RIFF") $ fail "missing RIFF header"
  _fileSize <- getWord32le
  wave <- getByteString 4
  unless (wave == "WAVE") $ fail "missing WAVE signature"
  chunks <- getChunks (WaveChunks Nothing Nothing)
  pure $ assemble chunks

getChunks :: WaveChunks -> Get WaveChunks
getChunks chunks = do
  done <- isEmpty
  if done
    then pure chunks
    else do
      chunkId <- getByteString 4
      chunkSize <- fromIntegral <$> getWord32le
      payload <- getByteString chunkSize
      when (odd chunkSize) $ do
        exhausted <- isEmpty
        unless exhausted $ () <$ getByteString 1
      let updated =
            case chunkId of
              "fmt " -> chunks {waveFormat = parseFormat payload}
              "data" -> chunks {waveData = Just payload}
              _ -> chunks
      getChunks updated

parseFormat :: ByteString -> Maybe WaveFormat
parseFormat payload =
  case runGetOrFail parser (Lazy.fromStrict payload) of
    Left _ -> Nothing
    Right (_, _, value) -> Just value
  where
    parser = do
      tag <- fromIntegral <$> getWord16le
      channels <- fromIntegral <$> getWord16le
      rate <- fromIntegral <$> getWord32le
      _byteRate <- getWord32le
      _blockAlign <- getWord16le
      bits <- fromIntegral <$> getWord16le
      pure $ WaveFormat tag channels rate bits

assemble :: WaveChunks -> Either Text WavAudio
assemble WaveChunks {waveFormat = Nothing} = Left "WAV file has no fmt chunk"
assemble WaveChunks {waveData = Nothing} = Left "WAV file has no data chunk"
assemble WaveChunks {waveFormat = Just format, waveData = Just payload}
  | formatChannels format <= 0 = Left "WAV channel count is zero"
  | otherwise = do
      interleaved <- decodeSamples format payload
      let channels = formatChannels format
          frameCount = Vector.length interleaved `div` channels
          mono =
            Vector.generate frameCount $ \frame ->
              let first = frame * channels
                  total = sum [interleaved Vector.! (first + channel) | channel <- [0 .. channels - 1]]
               in total / fromIntegral channels
      pure $ WavAudio (formatSampleRate format) mono

decodeSamples :: WaveFormat -> ByteString -> Either Text (Vector Float)
decodeSamples format payload =
  case (formatTag format, formatBitsPerSample format) of
    (1, 16) -> runSampleParser 2 (realToFrac . ((/ 32768) . fromIntegral :: Int16 -> Double)) getInt16le
    (1, 32) -> runSampleParser 4 (realToFrac . ((/ 2147483648) . fromIntegral :: Int32 -> Double)) getInt32le
    (3, 32) -> runSampleParser 4 id getFloatle
    pair -> Left $ "Unsupported WAV encoding: format/bits " <> Text.pack (show pair)
  where
    runSampleParser bytesPerSample convert parser
      | ByteString.length payload `mod` bytesPerSample /= 0 = Left "WAV data chunk has a partial sample"
      | otherwise =
          let count = ByteString.length payload `div` bytesPerSample
           in case runGetOrFail (Vector.replicateM count (convert <$> parser)) (Lazy.fromStrict payload) of
                Left (_, offset, message) -> Left $ "Invalid sample at byte " <> Text.pack (show offset) <> ": " <> Text.pack message
                Right (_, _, values) -> Right values

floatToInt16 :: Float -> Int16
floatToInt16 sample =
  let clamped = max (-1) (min 1 sample)
      scaled = if clamped < 0 then clamped * 32768 else clamped * 32767
   in round scaled
