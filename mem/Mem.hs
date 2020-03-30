module Main (main) where

import           Codec.Lzip           (compressFile, decompress)
import           Control.Concurrent   (forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)
main :: IO ()
main =
    compressDump *>
    decompressDump *>
    compressMultithreaded "gmp-6.1.2.tar"

decompressDump :: IO ()
decompressDump = withSystemTempDirectory "lz" $
    \fp -> BSL.writeFile (fp </> "gmp-6.1.2.tar") =<<
        (decompress <$> BSL.readFile "gmp-6.1.2.tar.lz")

compressDump :: IO ()
compressDump = withSystemTempDirectory "lz" $
    \fp -> BSL.writeFile (fp </> "gmp-6.1.2.tar.lz") =<<
        (compressFile "gmp-6.1.2.tar")

forceHead :: BSL.ByteString -> IO ()
forceHead bsl = BS.length (head $ BSL.toChunks bsl) `seq` mempty

forceBSL :: BSL.ByteString -> IO ()
forceBSL bsl = BS.length (last $ BSL.toChunks bsl) `seq` mempty

compressMultithreaded :: FilePath -> IO ()
compressMultithreaded fp = do
    bsl <- compressFile fp -- compress <$> BSL.readFile fp
    forceHead bsl
    done <- newEmptyMVar
    _ <- forkIO (forceBSL bsl *> putMVar done ())
    takeMVar done
