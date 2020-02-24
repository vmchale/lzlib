module Main (main) where

import           Codec.Lzip           (compress, decompress)
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((-<.>))
import           Test.Tasty
import           Test.Tasty.Golden

testDecompress :: FilePath -> TestTree
testDecompress fp =
    goldenVsString ("Decompress " ++ fp) (fp -<.> ".txt") (decompress <$> BSL.readFile fp)

testCompress :: FilePath -> TestTree
testCompress fp =
    goldenVsString ("Compress " ++ fp) (fp -<.> ".lz") (compress <$> BSL.readFile fp)

main :: IO ()
main =
    defaultMain $
        testGroup "bz2" [testDecompression, testCompression]

    where
        testDecompression = testGroup "Decompress"
            [ testDecompress "test/data/test.txt.lz" ]
        testCompression = testGroup "Compress"
            [ testDecompress "test/data/test.txt" ]
