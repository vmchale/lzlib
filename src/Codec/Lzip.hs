{-# LANGUAGE OverloadedStrings #-}

module Codec.Lzip ( compressStrict
                  , compressWithStrict
                  , decompressStrict
                  , CompressionLevel (..)
                  -- * Lower-level bindings
                  , module Codec.Lzip.Raw
                  ) where

import           Codec.Lzip.Raw
import           Control.Monad         (unless, void)
import           Data.Bits             (shiftL)
import qualified Data.ByteString       as BS
import           Data.Int              (Int64)
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr           (castPtr)
import           System.IO.Unsafe      (unsafePerformIO)

data CompressionLevel = Zero
                      | One
                      | Two
                      | Three
                      | Four
                      | Five
                      | Six
                      | Seven
                      | Eight
                      | Nine

data LzOptions = LzOptions { _dictionarySize :: !Int
                           , _matchLenLimit  :: !Int
                           }

encoderOptions :: CompressionLevel -> LzOptions
encoderOptions Zero  = LzOptions 65535 16
encoderOptions One   = LzOptions (1 `shiftL` 20) 5
encoderOptions Two   = LzOptions (3 `shiftL` 19) 6
encoderOptions Three = LzOptions (1 `shiftL` 21) 8
encoderOptions Four  = LzOptions (3 `shiftL` 20) 12
encoderOptions Five  = LzOptions (1 `shiftL` 22) 20
encoderOptions Six   = LzOptions (1 `shiftL` 23) 36
encoderOptions Seven = LzOptions (1 `shiftL` 24) 68
encoderOptions Eight = LzOptions (3 `shiftL` 23) 132
encoderOptions Nine  = LzOptions (1 `shiftL` 25) 273

{-# NOINLINE decompressStrict #-}
decompressStrict :: BS.ByteString -> BS.ByteString
decompressStrict bs = unsafePerformIO $ BS.useAsCStringLen bs $ \(bytes, sz) -> do

    decoder <- lZDecompressOpen

    void $ lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz)
    void $ lZDecompressFinish decoder

    readLoop decoder (4 * sz) "" <* lZDecompressClose decoder

    where
        readLoop :: LZDecoderPtr -> Int -> BS.ByteString -> IO BS.ByteString
        readLoop decoder sz acc = do

            newBytes <- mallocBytes sz
            bytesActual <- lZDecompressRead decoder newBytes (fromIntegral sz)

            res <- lZDecompressFinished decoder

            bsActual <- BS.packCStringLen (castPtr newBytes, fromIntegral bytesActual)
            free newBytes
            if res == 1
                then pure $ acc `BS.append` bsActual
                else readLoop decoder sz (acc `BS.append` bsActual)

{-# NOINLINE compressStrict #-}
compressStrict :: BS.ByteString -> BS.ByteString
compressStrict = compressWithStrict Nine

-- FIXME: memory?
{-# NOINLINE compressWithStrict #-}
compressWithStrict :: CompressionLevel -> BS.ByteString -> BS.ByteString
compressWithStrict level bs = unsafePerformIO $ BS.useAsCStringLen bs $ \(bytes, sz) -> do

    encoder <- lZCompressOpen (fromIntegral dictionarySize) (fromIntegral matchLenLimit) (fromIntegral memberSize)

    void $ lZCompressWrite encoder (castPtr bytes) (fromIntegral sz)
    void $ lZCompressFinish encoder

    -- this is stupid but eh
    newBytes <- mallocBytes sz
    bytesActual <- lZCompressRead encoder newBytes (fromIntegral sz)

    res <- lZCompressFinished encoder

    unless (res == 1) $
        error "Shouldn't happen (compressWithStrict)"

    void $ lZCompressClose encoder

    BS.packCStringLen (castPtr newBytes, fromIntegral bytesActual) <* free newBytes

    where

        memberSize :: Int64
        memberSize = maxBound

        dictionarySize = _dictionarySize $ encoderOptions level
        matchLenLimit = _matchLenLimit $ encoderOptions level
