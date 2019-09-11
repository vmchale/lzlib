module Codec.Lzip ( LZErrno (..)
                  , lZVersion
                  , lZStrerror
                  , LZEncoder
                  , LZEncoderPtr
                  , UInt8
                  -- * Compression functions
                  , lZCompressOpen
                  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr)

#include <stdint.h>
#include <lzlib.h>

{# enum LZ_Errno as LZErrno {underscoreToCase} #}

{# fun LZ_version as ^ {} -> `CString' #}
{# fun LZ_strerror as ^ { `LZErrno' } -> `CString' #}

-- | Abstract data type
data LZEncoder

{# pointer *LZ_Encoder as LZEncoderPtr -> LZEncoder #}

type UInt8 = {# type uint8_t #}

{# fun LZ_compress_open as ^ { `CInt', `CInt', id `CULLong' } -> `LZEncoderPtr' #}
{# fun LZ_compress_close as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_finish as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_restart_member as ^ { `LZEncoderPtr', id `CULLong' } -> `CInt' #}
{# fun LZ_compress_sync_flush as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_read as ^ { `LZEncoderPtr', id `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_compress_write as ^ { `LZEncoderPtr', id `Ptr UInt8', `CInt' } -> `CInt' #}
{# fun LZ_compress_write_size as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_errno as ^ { `LZEncoderPtr' } -> `LZErrno' #}
{# fun LZ_compress_finished as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_member_finished as ^ { `LZEncoderPtr' } -> `CInt' #}
{# fun LZ_compress_data_position as ^ { `LZEncoderPtr' } -> `CULLong' id #}
