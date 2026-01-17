module Main ( main ) where

import           Codec.Lzip                   (compressBest, decompress)
import           Data.ByteString.Pathological (nonstandardRead)
import           Test.Tasty                   (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit             (testCase, (@?=))

compressFile :: FilePath -> TestTree
compressFile fp = testCase fp $ do
    d <- nonstandardRead fp
    decompress (compressBest d) @?= d

main :: IO ()
main = defaultMain $
    testGroup "roundtrip"
        [ compressFile "test/data/7341e7190c2219dbc2641707c5c0e41e038f8a44.jpg" ]
