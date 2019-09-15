module Codec.Lzip ( compressStrict
                  , compressWithStrict
                  , decompressStrict
                  , CompressionLevel (..)
                  -- * Low-level bindings
                  , module Codec.Lzip.Raw
                  ) where

import           Codec.Lzip.Raw
import           Control.Monad         (unless, void, when)
import           Data.Bits             (shiftL)
import qualified Data.ByteString       as BS
import           Data.Int              (Int64)
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr           (Ptr, castPtr, plusPtr)
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

-- | This does not do any error recovery; for that you should use
-- [lziprecover](https://www.nongnu.org/lzip/lziprecover.html).
{-# NOINLINE decompressStrict #-}
decompressStrict :: BS.ByteString -> BS.ByteString
decompressStrict bs = unsafePerformIO $ BS.useAsCStringLen bs $ \(bytes, sz) -> do

    decoder <- lZDecompressOpen
    maxSz <- lZDecompressWriteSize decoder

    let bufMax = min (fromIntegral maxSz) sz
    buf <- mallocBytes sz
    res <- loop decoder (buf, bufMax) (castPtr bytes, sz) 0 sz mempty

    void $ lZDecompressClose decoder
    free buf

    pure res

    where
        loop :: LZDecoderPtr -> (Ptr UInt8, Int) -> (Ptr UInt8, Int) -> Int -> Int -> BS.ByteString -> IO BS.ByteString
        loop decoder (buf, bufSz) (bytes, sz) offset total acc = do
            let toWrite = min bufSz (total - offset)
            bytesWritten <- if offset < total
                then Just <$> lZDecompressWrite decoder (bytes `plusPtr` offset) (fromIntegral toWrite)
                else Nothing <$ lZDecompressFinish decoder
            res <- lZDecompressFinished decoder
            if res == 1
                then pure acc
                else do
                    let newOffset = offset + fromIntegral (fromMaybe 0 bytesWritten)
                    bytesRead <- lZDecompressRead decoder buf (fromIntegral bufSz)
                    bsActual <- BS.packCStringLen (castPtr buf, fromIntegral bytesRead)
                    loop decoder (buf, bufSz) (bytes, sz) newOffset total (acc <> bsActual)

{-# NOINLINE compressStrict #-}
compressStrict :: BS.ByteString -> BS.ByteString
compressStrict = compressWithStrict Nine

-- TODO: memory use
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
        error "Shouldn't happen"

    void $ lZCompressClose encoder

    BS.packCStringLen (castPtr newBytes, fromIntegral bytesActual) <* free newBytes

    where

        memberSize :: Int64
        memberSize = maxBound

        dictionarySize = _dictionarySize $ encoderOptions level
        matchLenLimit = _matchLenLimit $ encoderOptions level
