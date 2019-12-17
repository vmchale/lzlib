module Main (main) where

import           Codec.Lzip
import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)

roundtrip :: BSL.ByteString -> BSL.ByteString
roundtrip = compress . decompress

roundtrip' :: BSL.ByteString -> BSL.ByteString
roundtrip' = decompress . compress

unpack :: IO ()
unpack = withSystemTempDirectory "lzlib" $
    \fp -> BSL.writeFile (fp </> "lzlib.tar.lz") =<<
        (roundtrip <$> BSL.readFile "lzlib-1.10.tar.lz")

unpack' :: IO ()
unpack' = withSystemTempDirectory "lzlib" $
    \fp -> BSL.writeFile (fp </> "lzlib.tar") =<<
        (roundtrip' <$> BSL.readFile "lzlib-1.10.tar")

main :: IO ()
main =
    defaultMain [ env file $ \ f ->
                  bgroup "roundtrip"
                      [ bench "lzlib (lzlib)" $ nf roundtrip f
                      ]
                , bgroup "unpack"
                      [ bench "lzlib" $ nfIO unpack ]
                , env decompressed $ \ f ->
                  bgroup "roundtrip'"
                      [ bench "lzlib (lzlib)" $ nf roundtrip' f
                      ]
                , bgroup "unpack"
                      [ bench "lzlib" $ nfIO unpack' ]
                ]
    where file = BSL.readFile "lzlib-1.10.tar.lz"
          decompressed = BSL.readFile "lzlib-1.10.tar"
