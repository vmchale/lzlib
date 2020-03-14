{-# LANGUAGE DeriveDataTypeable #-}

-- | Consult the lzlib [documentation](https://www.nongnu.org/lzip/manual/lzlib_manual.html)
-- for more details
--
-- This library uses 'Foreign.ForeignPtr.ForeignPtr's; to convert a @'Ptr' 'LZDecoder'@ to a @'Foreign.ForeignPtr.ForeignPtr' 'LZDecoder'@, use 'Foreign.ForeignPtr.newForeignPtr'
module Codec.Lzip.Raw ( -- * Prolegomena
                        LZErrno (..)
                      , lZVersion
                      , lZMinDictionarySize
                      , UInt8
                      -- * Compression functions
                      , LZEncoder
                      , LZEncoderPtr
                      , lZCompressOpen
                      , lZCompressClose
                      , lZCompressFinish
                      , lZCompressRead
                      , lZCompressWrite
                      , lZCompressWriteSize
                      , lZCompressFinished
                      -- * Decompression functions
                      , LZDecoder
                      , LZDecoderPtr
                      , lZDecompressOpen
                      , lZDecompressClose
                      , lZDecompressFinish
                      , lZDecompressRead
                      , lZDecompressWrite
                      , lZDecompressWriteSize
                      , lZDecompressErrno
                      , lZDecompressFinished
                      -- * Macros
                      , lZApiVersion
                      ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Foreign.C.Types
import Foreign.Ptr (Ptr)

#include <stdint.h>
#include <lzlib.h>

-- | @since 0.3.1.0
lZApiVersion :: Integral a => a
lZApiVersion = {# const LZ_API_VERSION #}

type UInt8 = {# type uint8_t #}
{#typedef uint8_t UInt8#}
{#default in `Ptr UInt8' [uint8_t *] id#}

{# enum LZ_Errno as LZErrno {underscoreToCase} deriving (Eq, Typeable) #}

{# fun pure LZ_version as ^ {} -> `String' #}
{# fun pure LZ_strerror as ^ { `LZErrno' } -> `String' #}
{# fun pure LZ_min_dictionary_size as ^ {} -> `CInt' #}

instance Show LZErrno where
    show = lZStrerror

instance Exception LZErrno where

-- | Abstract data type
data LZEncoder

{# pointer *LZ_Encoder as LZEncoderPtr foreign finalizer LZ_compress_close as ^ -> LZEncoder #}

{# fun LZ_compress_open as ^ { `CInt', `CInt', id `CULLong' } -> `Ptr LZEncoder' id #}
{# fun LZ_compress_finish as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_read as ^ { `LZEncoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_compress_write as ^ { `LZEncoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_compress_write_size as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_finished as ^ { `LZEncoderPtr' } -> `CInt' #}

-- | Abstract data type
data LZDecoder

{# pointer *LZ_Decoder as LZDecoderPtr foreign finalizer LZ_decompress_close as ^ -> LZDecoder #}

{# fun LZ_decompress_open as ^ {} -> `Ptr LZDecoder' id #}
{# fun LZ_decompress_finish as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_read as ^ { `LZDecoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_decompress_write as ^ { `LZDecoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_decompress_write_size as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_errno as ^ { `LZDecoderPtr' } -> `LZErrno' #}
{# fun LZ_decompress_finished as ^ { `LZDecoderPtr' } -> `CInt' #}
