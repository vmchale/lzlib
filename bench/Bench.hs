module Main (main) where

import           Codec.Lzip
import           Criterion.Main
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)

roundtrip :: BS.ByteString -> BSL.ByteString
roundtrip = compress . BSL.toStrict . decompress

roundtrip' :: BS.ByteString -> BSL.ByteString
roundtrip' = decompress . BSL.toStrict . compress

unpack :: IO ()
unpack = withSystemTempDirectory "lzlib" $
    \fp -> BSL.writeFile (fp </> "lzlib.tar.lz") =<<
        (roundtrip <$> BS.readFile "lzlib-1.10.tar.lz")

unpack' :: IO ()
unpack' = withSystemTempDirectory "lzlib" $
    \fp -> BSL.writeFile (fp </> "lzlib.tar") =<<
        (roundtrip' <$> BS.readFile "lzlib-1.10.tar")

main :: IO ()
main =
    defaultMain [ env file $ \ f ->
                  bgroup "roundtrip"
                      [ bench "lzlib" $ nf roundtrip f ]
                , bgroup "unpack"
                      [ bench "lzlib" $ nfIO unpack ]
                , env decompressed $ \f ->
                  bgroup "roundtrip'"
                      [ bench "lzlib" $ nf roundtrip' f ]
                , bgroup "unpack"
                      [ bench "lzlib" $ nfIO unpack' ]
                ]
    where file = BS.readFile "lzlib-1.10.tar.lz"
          decompressed = BS.readFile "lzlib-1.10.tar"
