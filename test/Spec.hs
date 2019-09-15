module Main ( main ) where

import           Codec.Lzip
import qualified Data.ByteString              as BS
import           Data.Foldable                (traverse_)
import           System.Directory             (doesDirectoryExist)
import           System.FilePath              ((</>))
import           System.FilePattern.Directory (getDirectoryFiles)
import           Test.Hspec

compressFile :: FilePath -> Spec
compressFile fp = parallel $
    it "roundtrip should be identity" $ do
        str <- BS.readFile fp
        decompressStrict (compressStrict str) `shouldBe` str

main :: IO ()
main = do
    b <- doesDirectoryExist "dist-newstyle"
    libs <- if b
        then getDirectoryFiles "dist-newstyle" ["**/*.so", "**/*.dll"]
        else pure []
    hspec $ do
        describe "decompress/compress" $
            traverse_ compressFile (("dist-newstyle" </>) <$> libs)
