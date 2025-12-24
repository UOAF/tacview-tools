module Data.Tacview.Sink (sink, sinkZip) where

import Codec.Archive.Zip
import Conduit
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf)
import Data.Tacview.Annotate
import Data.Tacview (ParsedLine, showLine, zipExt, txtExt)
import Data.Text qualified as T
import System.IO

-- | Write everything out when we're done.
sink :: Channel c => Maybe FilePath -> IO (c ParsedLine -> IO (), IORef Integer)
sink mfp = do
    iow <- newIORef 0
    to <- sinkStream mfp
    let srcC source = repeatMC (liftIO $ atomically (readChannel source))
            .| mapWhileC id
            .| mapC showLine
            .| iterMC (const . liftIO $ atomicModifyIORef' iow $ \p -> (p + 1, ()))
            .| unlinesC
            .| encodeUtf8C
        sinker src = to $ srcC src
    pure (sinker, iow)

-- | Writes already-printed lines to a ZIP file without progress; useful for the server.
sinkZip :: Channel c => FilePath -> IO (c T.Text -> IO ())
sinkZip fp = do
    let srcC source = repeatMC (liftIO $ atomically (readChannel source))
            .| mapWhileC id
            .| unlinesC
            .| encodeUtf8C
        sinker src = writeZip' fp $ srcC src
    pure sinker

sinkStream :: Maybe FilePath -> IO (ConduitT () BS.ByteString (ResourceT IO) () -> IO ())
sinkStream = \case
    Nothing -> pure writeStdout
    Just fp -> go where
        go
            | zipExt `isSuffixOf` fp = pure $ writeZip fp
            | txtExt `isSuffixOf` fp = pure $ writeTxt fp
            | otherwise = fail "expected a .zip.acmi or .txt.acmi file"

writeStdout :: ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeStdout src = whileIO "while writing to stdout" $ runConduitRes $ src .| sinkHandle stdout

writeTxt :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeTxt t src = whileIO ("while writing to " <> tn) $ runConduitRes $ src .| sinkFile tn where
    tn = (T.unpack . T.dropEnd (length txtExt) . T.pack $ t) <> "-filtered" <> txtExt

writeZip :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeZip z = writeZip' zn where
    zn = (T.unpack . T.dropEnd (length zipExt) . T.pack $ z) <> "-filtered" <> zipExt

writeZip' :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeZip' z src = whileIO ("while writing to " <> z) $ do
    sel <- mkEntrySelector "acmi.txt"
    createArchive z $ sinkEntry Deflate src sel
