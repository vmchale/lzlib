module Main ( main ) where

import           Codec.Lzip
import           Control.Monad        (filterM)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (traverse_)
import           System.Directory     (doesFileExist)
import           Test.Hspec

compressFile :: FilePath -> Spec
compressFile fp = parallel $
    it "roundtrip should be identity" $ do
        str <- BSL.readFile fp
        decompress (compress str) `shouldBe` str

roundtripFile :: FilePath -> Spec
roundtripFile fp = parallel $
    it "roundtrip should be identity" $ do
        str <- BSL.readFile fp
        compress (decompress str) `shouldBe` str

main :: IO ()
main = do
    ex' <- filterM doesFileExist ["gmp-6.1.2.tar.lz", "lzlib-1.10.tar.lz"]
    ex <- filterM doesFileExist ["gmp-6.1.2.tar", "lzlib-1.10.tar"]
    hspec $ do
        describe "decompress/compress" $
            traverse_ compressFile ex
        describe "compress/decompress" $
            traverse_ roundtripFile ex'
