module Main ( main ) where

import           Codec.Lzip
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import           Data.Foldable                (traverse_)
import           System.Directory             (doesDirectoryExist)
import           System.FilePath              ((</>))
import           System.FilePattern.Directory (getDirectoryFiles)
import           Test.Hspec

compressFile :: FilePath -> Spec
compressFile fp = parallel $
    it "roundtrip should be identity" $ do
        str <- BS.readFile fp
        decompress (BSL.toStrict (compress str)) `shouldBe` (BSL.fromStrict str)

roundtripFile :: FilePath -> Spec
roundtripFile fp = parallel $
    it "roundtrip should be identity" $ do
        str <- BS.readFile fp
        compress (BSL.toStrict (decompress str)) `shouldBe` (BSL.fromStrict str)

main :: IO ()
main = hspec $ do
    describe "decompress/compress" $
        traverse_ compressFile ["gmp-6.1.2.tar", "lzlib-1.10.tar"]
    describe "compress/decompress" $
        traverse_ roundtripFile ["gmp-6.1.2.tar.lz", "lzlib-1.10.tar.lz"]
