module Main (main) where

import           Codec.Lzip
import           Control.Applicative  ((<$>))
import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)

unpack :: FilePath -> IO ()
unpack fp' = withSystemTempDirectory "lzlib" $
    \fp -> BSL.writeFile (fp </> "dump.tar") =<<
        (decompress <$> BSL.readFile fp')

pack :: FilePath -> IO ()
pack fp' = withSystemTempDirectory "lzlib" $
    \fp -> BSL.writeFile (fp </> "dump.tar.lz") =<<
        compressFile fp'

main :: IO ()
main =
    defaultMain [ bgroup "unpack"
                      [ bench "lzlib" $ nfIO (unpack "lzlib-1.10.tar.lz")
                      , bench "lzlib" $ nfIO (unpack "gmp-6.1.2.tar.lz")
                      ]
                , bgroup "pack"
                      [ bench "lzlib" $ nfIO (pack "lzlib-1.10.tar") ]
                ]
