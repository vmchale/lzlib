{-# LANGUAGE BangPatterns #-}

module Codec.Lzip ( compress
                  , compressWith
                  , decompress
                  , CompressionLevel (..)
                  -- * Low-level bindings
                  , module Codec.Lzip.Raw
                  ) where

import           Codec.Lzip.Raw
import           Control.Exception      (bracket)
import           Control.Monad          (void, when)
import           Data.Bits              (shiftL)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Unsafe as BS
import           Data.Functor           (($>))
import           Data.Int               (Int64)
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Ptr            (Ptr, castPtr)
import           System.IO.Unsafe       (unsafeDupablePerformIO)

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
--
-- Doesn't work on empty 'BSL.ByteString's
{-# NOINLINE decompress #-}
decompress :: BSL.ByteString -> BSL.ByteString
decompress bs = unsafeDupablePerformIO $ do

    let bss = BSL.toChunks bs
        szOut = 32 * 1024

    let setup = do
            decoder <- lZDecompressOpen
            maxSz <- lZDecompressWriteSize decoder
            let bufMax = min (32 * 1024) (fromIntegral maxSz)
            buf <- mallocBytes szOut
            pure (decoder, buf, bufMax)

    let cleanup (decoder, buf, _) =
            lZDecompressClose decoder *>
            free buf

    BSL.fromChunks <$> bracket
        setup
        cleanup
        (\(decoder, buf, bufMax) -> loop decoder bss bufMax (buf, szOut) mempty)

    where
        loop :: LZDecoderPtr -> [BS.ByteString] -> Int -> (Ptr UInt8, Int) -> [BS.ByteString] -> IO [BS.ByteString]
        loop decoder bss maxSz (buf, bufSz) !acc = do
            bss' <- case bss of
                [bs'] -> if BS.length bs' > maxSz
                    then do
                        let (bs'', rest) = BS.splitAt maxSz bs'
                        BS.useAsCStringLen bs'' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) $> [rest]
                    else
                        BS.useAsCStringLen bs' $ \(bytes, sz) -> do
                            void $ lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz)
                            void $ lZDecompressFinish decoder
                            pure []
                (bs':bss') -> if BS.length bs' > maxSz
                    then do
                        let (bs'', rest) = BS.splitAt maxSz bs'
                        BS.unsafeUseAsCStringLen bs'' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) $> rest:bss'
                    else
                        BS.unsafeUseAsCStringLen bs' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) $>
                            bss'
                [] -> pure []

            res <- lZDecompressFinished decoder
            if res == 1
                then pure acc
                else do
                    bytesRead <- lZDecompressRead decoder buf (fromIntegral bufSz)
                    when (bytesRead == -1) $
                        error . show =<< lZDecompressErrno decoder
                    bsActual <- BS.packCStringLen (castPtr buf, fromIntegral bytesRead)
                    loop decoder bss' maxSz (buf, bufSz) (acc ++ [bsActual])

-- | Defaults to 'Nine'
{-# NOINLINE compress #-}
compress :: BSL.ByteString -> BSL.ByteString
compress = compressWith Nine

{-# NOINLINE compressWith #-}
compressWith :: CompressionLevel -> BSL.ByteString -> BSL.ByteString
compressWith level bstr = unsafeDupablePerformIO $ do

    let bss = BSL.toChunks bstr
        sz = fromIntegral (BSL.length bstr)
        delta = sz `div` 4 + 64

    let setup = do
            encoder <- lZCompressOpen (fromIntegral $ dictionarySize sz) (fromIntegral matchLenLimit) (fromIntegral memberSize)
            newBytes <- mallocBytes delta
            pure (encoder, newBytes)

    let cleanup (encoder, newBytes) =
            lZCompressClose encoder *>
            free newBytes

    BSL.fromChunks <$> bracket
        setup
        cleanup
        (\(encoder, newBytes) -> loop encoder bss (newBytes, delta) 0 mempty)

    where
        loop :: LZEncoderPtr -> [BS.ByteString] -> (Ptr UInt8, Int) -> Int -> [BS.ByteString] -> IO [BS.ByteString]
        loop encoder bss (buf, sz) bytesRead acc = do
            bss' <- case bss of
                [bs] -> BS.unsafeUseAsCStringLen bs $ \(bytes, sz') -> do
                    void $ lZCompressWrite encoder (castPtr bytes) (fromIntegral sz')
                    lZCompressFinish encoder $> []
                (bs:bss') -> BS.unsafeUseAsCStringLen bs $ \(bytes, sz') ->
                    lZCompressWrite encoder (castPtr bytes) (fromIntegral sz') $> bss'
                [] -> pure []
            bytesActual <- lZCompressRead encoder buf (fromIntegral sz)
            res <- lZCompressFinished encoder
            bsActual <- BS.packCStringLen (castPtr buf, fromIntegral bytesActual)
            if res == 1
                then pure (acc ++ [bsActual])
                else loop encoder bss' (buf, sz) (bytesRead + fromIntegral bytesActual) (acc ++ [bsActual])

        memberSize :: Int64
        memberSize = maxBound

        -- saves memory
        dictionarySize = max (fromIntegral lZMinDictionarySize) . min (_dictionarySize $ encoderOptions level)
        matchLenLimit = _matchLenLimit opts
        opts = encoderOptions level
