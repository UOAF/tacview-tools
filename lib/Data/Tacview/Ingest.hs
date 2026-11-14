-- | Common input pipeline for server and filter
--
-- While how we actually delta-encode and filter differs slightly when serving many clients,
-- we read from stdin/files in the same way and want to pass through a common set of filters.
module Data.Tacview.Ingest where

import Control.Concurrent.Channel.Metrics (ReaderMetrics)
import Control.Concurrent.Channel.Split
import Control.Concurrent.TBCQueue
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Tacview
import Data.Tacview.Ignores
import Data.Tacview.MinId
import Data.Tacview.Source

data ShouldMinifyIds = MinifiedIds | OriginalIds deriving stock (Eq)

data IngestResults = IngestResults {
    linesFiltered :: FilteredLines,
    -- | Each stage's name and reader metrics, in pipeline order
    stageMetrics :: [(String, ReaderMetrics)]
}

ingest
    :: ShouldMinifyIds
    -> Maybe FilePath
    -> IO (TBCQWriter ParsedLine -> IO IngestResults, Maybe SourceLength, SourceProgress)
ingest mini inputPath = do
    (src, mlen, readProgress) <- source inputPath
    let pipe = pipeline (newTBCQueueIO 1024)
        ignore sink = do
            (filtered, m) <- snd <$> pipe src (`filterLines` sink)
            pure $ IngestResults filtered [("parsing and filtering", m)]
        -- Now that we're dealing with `Reader ParsedLine -> Writer ParsedLine -> IO ReaderMetrics`
        -- filters, we can chain them in some uniform way.
        -- (For now it's just minifying, but you can imagine further transforms, like relabeling.)
        chain
            :: String
            -> (TBCQReader a -> TBCQWriter a -> IO ReaderMetrics)
            -> State (TBCQWriter a -> IO IngestResults) ()
        chain name next = modify $ chain' name next
        chain' name next prev sink = do
            (r, m) <- pipe prev (`next` sink)
            pure r { stageMetrics = r.stageMetrics <> [(name, m)] }
        finalChain = flip execState ignore $ do
            when (mini == MinifiedIds) $ chain "ID minification" minId
            -- ...and so on for more filters.

    pure (finalChain, mlen, readProgress)
