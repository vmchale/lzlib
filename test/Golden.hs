module Main (main) where

import           Codec.Lzip
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      (dropExtension, (<.>))
import           Test.Tasty
import           Test.Tasty.Golden

testDecompress :: FilePath -> TestTree
testDecompress fp =
    goldenVsString ("Decompress " ++ fp) (dropExtension fp) (decompress <$> BSL.readFile fp)

main :: IO ()
main =
    defaultMain $
        testGroup "bz2" [testDecompression]

    where
        testDecompression = testGroup "Decompress"
            [ testDecompress "test/data/test.txt.lz" ]
