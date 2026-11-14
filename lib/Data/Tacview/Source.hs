module Data.Tacview.Source (
    source,
    SourceLength (..),
    SourceProgress(..)
) where

import Codec.Archive.Zip
import Conduit
import Control.Concurrent.Channel (Channel)
import Control.Concurrent.Channel.Split
import Control.Concurrent.STM
import Control.Monad
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Tacview (zipExt, txtExt)
import Data.Tacview.Annotate
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory (getFileSize)
import System.IO

type ByteConduit = ConduitT () BS.ByteString (ResourceT IO) ()

newtype SourceLength = SourceLength { unLength :: Integer }

sourceC :: Maybe FilePath -> IO (ByteConduit, Maybe SourceLength)
sourceC = \case
    Nothing -> pure (readStdin, Nothing)
    Just fp -> second Just <$> go where
        go
            | zipExt `isSuffixOf` fp = readZip fp
            | txtExt `isSuffixOf` fp = readTxt fp
            | otherwise = fail "expected a .zip.acmi or .txt.acmi file"

readStdin :: ByteConduit
readStdin = sourceHandle stdin

readTxt :: FilePath -> IO (ByteConduit, SourceLength)
readTxt fp = do
    len <- getFileSize fp
    pure (sourceFile fp, SourceLength len)

readZip :: FilePath -> IO (ByteConduit, SourceLength)
readZip z = withArchive z $ do
    e <- getEntries
    when (M.null e) $ error "empty ZIP archive!"
    let (sel, des) = head $ M.toList e
    src <- getEntrySource sel
    pure (src, SourceLength $ fromIntegral des.edUncompressedSize)

data SourceProgress = SourceProgress {
    bytes:: IORef Integer,
    lines :: IORef Integer
}

source :: Channel c
    => Maybe FilePath
    -> IO (Writer c Text -> IO (), Maybe SourceLength, SourceProgress)
source mfp = do
    (srcC, len) <- sourceC mfp
    prog <- SourceProgress <$> newIORef 0 <*> newIORef 0
    let countBytes b = liftIO $
            atomicModifyIORef' prog.bytes $ \p -> (p + fromIntegral (BS.length b), ())
        go c l = do
            atomically $ writeChannel' c l
            atomicModifyIORef' prog.lines $ \p -> (p + 1, ())
        run c = whileIO ("while reading " <> fromMaybe "stdin" mfp) $ runConduitRes $
            srcC .| iterMC countBytes .| decodeLinesC .| mapM_C (liftIO . go c)
    pure (run, len, prog)

-- | Splits UTF-8 bytes into lines, like @decodeUtf8C .| linesUnboundedC@.
--
-- linesUnboundedC gets 'T.break' through a typeclass dictionary,
-- so it calls an unknown predicate (and allocates a boxed 'Char') for each character.
-- That costs more than all our parsing. 'T.lines' uses memchr instead.
decodeLinesC :: MonadThrow m => ConduitT BS.ByteString Text m ()
decodeLinesC = loop BS.empty where
    loop partial = await >>= \case
        Nothing -> unless (BS.null partial) $ decode partial >>= yield
        Just b -> case BS.elemIndexEnd 10 b of
            Nothing -> loop (partial <> b)
            Just i -> do
                let (complete, rest) = BS.splitAt (i + 1) b
                decode (partial <> complete) >>= yieldMany . T.lines
                loop rest
    decode :: MonadThrow m => BS.ByteString -> m Text
    decode = either throwM pure . T.decodeUtf8'
