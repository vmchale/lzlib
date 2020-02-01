{-# LANGUAGE TupleSections #-}

module Codec.Lzip ( compress
                  , compressBest
                  , compressFast
                  , compressWith
                  , decompress
                  , CompressionLevel (..)
                  -- * Low-level bindings
                  , module Codec.Lzip.Raw
                  ) where

import           Codec.Lzip.Raw
import           Control.Applicative
import           Control.Exception            (throw)
import           Control.Monad                (when)
import           Control.Monad.ST.Lazy        (runST)
import qualified Control.Monad.ST.Lazy        as LazyST
import qualified Control.Monad.ST.Lazy.Unsafe as LazyST
import           Data.Bits                    (shiftL)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Unsafe       as BS
import           Data.Functor                 (($>))
import           Data.Int                     (Int64)
import           Foreign.C.Types              (CInt)
import           Foreign.ForeignPtr           (ForeignPtr, castForeignPtr,
                                               mallocForeignPtrBytes,
                                               newForeignPtr, withForeignPtr)
import           Foreign.Ptr                  (Ptr, castPtr)
import           System.IO.Unsafe

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

data LzOptions = LzOptions
    { _dictionarySize :: !Int
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
decompress :: BSL.ByteString -> BSL.ByteString
decompress bs = runST $ do

    let bss = BSL.toChunks bs
        szOut :: Integral a => a
        szOut = 32 * 1024

    (dec, bufOut) <- LazyST.unsafeIOToST $ do
        bufOut <- mallocForeignPtrBytes szOut
        decoder <- lZDecompressOpen
        dec <- newForeignPtr lZDecompressClose (castPtr decoder)
        pure (dec, bufOut)

    BSL.fromChunks <$> loop (castForeignPtr dec) bss (bufOut, szOut)

    where

        -- TODO: not a fan of this loop!
        step :: LZDecoderPtr -> [BS.ByteString] -> (ForeignPtr UInt8, CInt) -> LazyST.ST s (Maybe BS.ByteString, [BS.ByteString])
        step decoder bss (buf, bufSz) = LazyST.unsafeIOToST $ do
            maxSz <- fromIntegral <$> lZDecompressWriteSize decoder
            bss' <- case bss of
                [bs'] -> if BS.length bs' > maxSz
                    then
                        let (bs'', rest) = BS.splitAt maxSz bs' in
                        BS.unsafeUseAsCStringLen bs'' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) $> [rest]
                    else
                        BS.unsafeUseAsCStringLen bs' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) *>
                            lZDecompressFinish decoder $> []
                (bs':bss') -> if BS.length bs' > maxSz
                    then
                        let (bs'', rest) = BS.splitAt maxSz bs' in
                        BS.unsafeUseAsCStringLen bs'' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) $> rest:bss'
                    else
                        BS.unsafeUseAsCStringLen bs' $ \(bytes, sz) ->
                            lZDecompressWrite decoder (castPtr bytes) (fromIntegral sz) $>
                            bss'
                [] -> pure []

            res <- lZDecompressFinished decoder
            if res == 1
                then pure (Nothing, bss')
                else
                    withForeignPtr buf $ \b -> do
                        bytesRead <- lZDecompressRead decoder b bufSz
                        when (bytesRead == -1) $
                            throw =<< lZDecompressErrno decoder
                        (, bss') . Just <$> BS.packCStringLen (castPtr b, fromIntegral bytesRead)

        loop :: LZDecoderPtr -> [BS.ByteString] -> (ForeignPtr UInt8, CInt) -> LazyST.ST s [BS.ByteString]
        loop decoder bss bufOut = do
            (res, bss') <- step decoder bss bufOut
            case res of
                Nothing -> pure []
                Just x  -> (x:) <$> loop decoder bss' bufOut

-- | Defaults to 'Six'
{-# NOINLINE compress #-}
compress :: BSL.ByteString -> BSL.ByteString
compress = compressWith Six

-- | Alias for @'compressWith' 'Nine'@
--
-- @since 0.3.2.0
{-# NOINLINE compressBest #-}
compressBest :: BSL.ByteString -> BSL.ByteString
compressBest = compressWith Nine

-- | Alias for @'compressWith' 'Zero'@
--
-- @since 0.3.2.0
{-# NOINLINE compressFast #-}
compressFast :: BSL.ByteString -> BSL.ByteString
compressFast = compressWith Zero

{-# NOINLINE compressWith #-}
compressWith :: CompressionLevel -> BSL.ByteString -> BSL.ByteString
compressWith level bstr = unsafeDupablePerformIO $ do

    let bss = BSL.toChunks bstr
        sz = fromIntegral (BSL.length bstr)
        delta = sz `div` 4 + 64

    buf <- mallocForeignPtrBytes delta
    withForeignPtr buf $ \newBytes -> do

        encoder <- lZCompressOpen (fromIntegral $ dictionarySize sz) (fromIntegral matchLenLimit) (fromIntegral memberSize)
        enc <- newForeignPtr lZCompressClose (castPtr encoder)
        BSL.fromChunks <$> loop (castForeignPtr enc) bss (newBytes, delta) 0

    where
        loop :: LZEncoderPtr -> [BS.ByteString] -> (Ptr UInt8, Int) -> Int -> IO [BS.ByteString]
        loop encoder bss (buf, sz) bytesRead = do
            maxSz <- fromIntegral <$> lZCompressWriteSize encoder
            bss' <- case bss of
                [bs] -> if BS.length bs > maxSz
                    then
                        let (bs', rest) = BS.splitAt maxSz bs in
                            BS.unsafeUseAsCStringLen bs' $ \(bytes, sz') ->
                                lZCompressWrite encoder (castPtr bytes) (fromIntegral sz') *>
                                lZCompressFinish encoder $> [rest]
                    else
                        BS.unsafeUseAsCStringLen bs $ \(bytes, sz') ->
                            lZCompressWrite encoder (castPtr bytes) (fromIntegral sz') *>
                            lZCompressFinish encoder $> []
                (bs:bss') -> if BS.length bs > maxSz
                    then
                        let (bs', rest) = BS.splitAt maxSz bs in
                        BS.unsafeUseAsCStringLen bs' $ \(bytes, sz') ->
                            lZCompressWrite encoder (castPtr bytes) (fromIntegral sz') $> (rest:bss')
                    else
                        BS.unsafeUseAsCStringLen bs $ \(bytes, sz') ->
                            lZCompressWrite encoder (castPtr bytes) (fromIntegral sz') $> bss'
                [] -> pure []
            bytesActual <- lZCompressRead encoder buf (fromIntegral sz)
            res <- lZCompressFinished encoder
            bsActual <- BS.packCStringLen (castPtr buf, fromIntegral bytesActual)
            if res == 1
                then pure [bsActual]
                else (bsActual:) <$> loop encoder bss' (buf, sz) (bytesRead + fromIntegral bytesActual)

        memberSize :: Int64
        memberSize = maxBound

        -- saves memory
        dictionarySize = max (fromIntegral lZMinDictionarySize) . min (_dictionarySize $ encoderOptions level)
        matchLenLimit = _matchLenLimit opts
        opts = encoderOptions level
