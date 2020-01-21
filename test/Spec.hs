module Main ( main ) where

import           Codec.Lzip
import           Control.Monad        (filterM)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (traverse_)
import           System.Directory     (doesFileExist)
import           Test.Hspec

nonstandardRead :: FilePath -> IO BSL.ByteString
nonstandardRead fp = do
    bStrict <- BS.readFile fp
    let (h, t) = BS.splitAt (64 * 1024) bStrict
    pure $ BSL.fromChunks [h, t]

compressFileGeneral :: (FilePath -> IO BSL.ByteString) -> FilePath -> Spec
compressFileGeneral f fp = parallel $
    it ("decompress . compress should be identity (" ++ fp ++ ")") $ do
        str <- f fp
        decompress (compressBest str) `shouldBe` str

compressFileFreaky :: FilePath -> Spec
compressFileFreaky = compressFileGeneral nonstandardRead

compressFile :: FilePath -> Spec
compressFile = compressFileGeneral BSL.readFile

decompressFileGeneral :: (FilePath -> IO BSL.ByteString) -> FilePath -> Spec
decompressFileGeneral f fp = parallel $
    it ("compress . decompress should be identity (" ++ fp ++ ")") $ do
        str <- f fp
        compressBest (decompress str) `shouldBe` str

decompressFileFreaky :: FilePath -> Spec
decompressFileFreaky = decompressFileGeneral nonstandardRead

decompressFile :: FilePath -> Spec
decompressFile = decompressFileGeneral BSL.readFile

main :: IO ()
main = do
    ex' <- filterM doesFileExist ["gmp-6.1.2.tar.lz", "lzlib-1.10.tar.lz"]
    ex <- filterM doesFileExist ["gmp-6.1.2.tar", "lzlib-1.10.tar"]
    hspec $ do
        describe "roundtrip" $
            traverse_ compressFile ex
        describe "roundtrip (sketchy)" $
            traverse_ compressFileFreaky ex
        describe "roundtrip" $
            traverse_ decompressFile ex'
        describe "roundtrip (sketchy)" $
            traverse_ decompressFileFreaky ex'
