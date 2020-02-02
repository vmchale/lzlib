{-# LANGUAGE DeriveDataTypeable #-}

-- | Consult the lzlib [documentation](https://www.nongnu.org/lzip/manual/lzlib_manual.html)
-- for more details
--
-- This library uses 'Foreign.ForeignPtr.ForeignPtr's; to convert a @'Ptr' 'LZDecoder'@ to a @'Foreign.ForeignPtr.ForeignPtr' 'LZDecoder'@, use 'Foreign.ForeignPtr.newForeignPtr'
module Codec.Lzip.Raw ( -- * Prolegomena
                        LZErrno (..)
                      , lZVersion
                      , lZStrerror
                      , lZMinDictionaryBits
                      , lZMinDictionarySize
                      , lZMaxDictionaryBits
                      , lZMaxDictionarySize
                      , lZMinMatchLenLimit
                      , lZMaxMatchLenLimit
                      , UInt8
                      -- * Compression functions
                      , LZEncoder
                      , LZEncoderPtr
                      , lZCompressOpen
                      , lZCompressClose
                      , lZCompressFinish
                      , lZCompressRestartMember
                      , lZCompressSyncFlush
                      , lZCompressRead
                      , lZCompressWrite
                      , lZCompressWriteSize
                      , lZCompressErrno
                      , lZCompressFinished
                      , lZCompressMemberFinished
                      , lZCompressDataPosition
                      , lZCompressMemberPosition
                      , lZCompressTotalInSize
                      , lZCompressTotalOutSize
                      -- * Decompression functions
                      , LZDecoder
                      , LZDecoderPtr
                      , lZDecompressOpen
                      , lZDecompressClose
                      , lZDecompressFinish
                      , lZDecompressReset
                      , lZDecompressSyncToMember
                      , lZDecompressRead
                      , lZDecompressWrite
                      , lZDecompressWriteSize
                      , lZDecompressErrno
                      , lZDecompressFinished
                      , lZDecompressMemberFinished
                      , lZDecompressDictionarySize
                      , lZDecompressDataCrc
                      , lZDecompressDataPosition
                      , lZDecompressMemberPosition
                      , lZDecompressTotalInSize
                      , lZDecompressTotalOutSize
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
{# fun pure LZ_min_dictionary_bits as ^ {} -> `CInt' #}
{# fun pure LZ_min_dictionary_size as ^ {} -> `CInt' #}
{# fun pure LZ_max_dictionary_bits as ^ {} -> `CInt' #}
{# fun pure LZ_max_dictionary_size as ^ {} -> `CInt' #}
{# fun pure LZ_min_match_len_limit as ^ {} -> `CInt' #}
{# fun pure LZ_max_match_len_limit as ^ {} -> `CInt' #}

instance Show LZErrno where
    show = lZStrerror

instance Exception LZErrno where

-- | Abstract data type
data LZEncoder

{# pointer *LZ_Encoder as LZEncoderPtr foreign finalizer LZ_compress_close as ^ -> LZEncoder #}

{# fun LZ_compress_open as ^ { `CInt', `CInt', id `CULLong' } -> `Ptr LZEncoder' id #}
{# fun LZ_compress_finish as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_restart_member as ^ { `LZEncoderPtr', id `CULLong' } -> `CInt' #}
{# fun LZ_compress_sync_flush as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_read as ^ { `LZEncoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_compress_write as ^ { `LZEncoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_compress_write_size as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_errno as ^ { `LZEncoderPtr' } -> `LZErrno' #}
{# fun LZ_compress_finished as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_member_finished as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_data_position as ^ { `LZEncoderPtr' } -> `CULLong' id #}
{# fun LZ_compress_member_position as ^ { `LZEncoderPtr' } -> `CULLong' id #}
{# fun LZ_compress_total_in_size as ^ { `LZEncoderPtr' } -> `CULLong' id #}
{# fun LZ_compress_total_out_size as ^ { `LZEncoderPtr' } -> `CULLong' id #}

-- | Abstract data type
data LZDecoder

{# pointer *LZ_Decoder as LZDecoderPtr foreign finalizer LZ_decompress_close as ^ -> LZDecoder #}

{# fun LZ_decompress_open as ^ {} -> `Ptr LZDecoder' id #}
{# fun LZ_decompress_finish as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_reset as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_sync_to_member as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_read as ^ { `LZDecoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_decompress_write as ^ { `LZDecoderPtr', `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_decompress_write_size as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_errno as ^ { `LZDecoderPtr' } -> `LZErrno' #}
{# fun LZ_decompress_finished as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_member_finished as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_dictionary_size as ^ { `LZDecoderPtr' } -> `CInt' #}
{# fun LZ_decompress_data_crc as ^ { `LZDecoderPtr' } -> `CUInt' #}
{# fun LZ_decompress_data_position as ^ { `LZDecoderPtr' } -> `CULLong' id #}
{# fun LZ_decompress_member_position as ^ { `LZDecoderPtr' } -> `CULLong' id #}
{# fun LZ_decompress_total_in_size as ^ { `LZDecoderPtr' } -> `CULLong' id #}
{# fun LZ_decompress_total_out_size as ^ { `LZDecoderPtr' } -> `CULLong' id #}
