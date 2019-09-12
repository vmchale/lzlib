module Codec.Lzip ( compress
                  -- , decompress
                  , CompressionLevel (..)
                  ) where

import           Codec.Lzip.Raw
import           Data.Bits             (shiftL)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Int              (Int64)
import           Foreign.C.String
import           Foreign.Marshal.Alloc (mallocBytes)

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

compress :: CStringLen -> IO CStringLen
compress = compressWith Nine

compressWith :: CompressionLevel -> CStringLen -> IO CStringLen
compressWith level (bytes, sz) = do

    encoder <- lZCompressOpen (fromIntegral dictionarySize) (fromIntegral matchLenLimit) (fromIntegral memberSize)

    let newDataSize = deltaSize

    newData <- mallocBytes newDataSize

    pure undefined

    where

        memberSize :: Int64
        memberSize = maxBound

        deltaSize :: Int
        deltaSize = sz `div` 4 + 64

        dictionarySize = _dictionarySize $ encoderOptions level
        matchLenLimit = _matchLenLimit $ encoderOptions level
