module Main ( main ) where

import           Codec.Lzip
import qualified Data.ByteString              as BS
import           Data.Foldable                (traverse_)
import           System.FilePath              ((</>))
import           System.FilePattern.Directory (getDirectoryFiles)
import           Test.Hspec
import System.Directory (doesDirectoryExist)

compressFile :: FilePath -> Spec
compressFile fp = parallel $
    it "roundtrip should be identity" $ do
        str <- BS.readFile fp
        decompressStrict (compressStrict str) `shouldBe` str

main :: IO ()
main = do
    -- todo: check dist-newstyle exists
    b <- doesDirectoryExist "dist-newstyle"
    libs <- if b
        then getDirectoryFiles "dist-newstyle" ["**/*.so", "**/*.dll"]
        else pure []
    hspec $
        describe "decompress/compress" $
            traverse_ compressFile (("dist-newstyle" </>) <$> libs)
