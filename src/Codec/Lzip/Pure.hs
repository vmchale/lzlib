module Codec.Lzip.Pure ( Member (..)
                       ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Word            (Word32, Word64, Word8)

-- see: https://www.nongnu.org/lzip/manual/lzip_manual.html#File-format
data Member =
    Member !Word8 !Word8 BSL.ByteString Word32 !Word64 Word64
-- for some reason this is little endian
--
-- pure bz2? huffman coding!
-- https://www.sourceware.org/bzip2/manual/manual.html#limits
