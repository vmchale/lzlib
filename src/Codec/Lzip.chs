module Codec.Lzip ( LZErrno (..) 
                  ) where

#include <lzlib.h>

{# enum LZ_Errno as LZErrno {underscoreToCase} #}
