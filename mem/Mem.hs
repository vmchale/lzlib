module Main (main) where

import           Codec.Lzip           (compressFile, decompress)
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)

main :: IO ()
main =
    compressDump *>
    decompressDump

decompressDump :: IO ()
decompressDump = withSystemTempDirectory "lz" $
    \fp -> BSL.writeFile (fp </> "gmp-6.1.2.tar") =<<
        (decompress <$> BSL.readFile "gmp-6.1.2.tar.lz")

compressDump :: IO ()
compressDump = withSystemTempDirectory "lz" $
    \fp -> BSL.writeFile (fp </> "gmp-6.1.2.tar.lz") =<<
        (compressFile "gmp-6.1.2.tar")
