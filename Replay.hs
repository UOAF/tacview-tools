module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Channel
import Control.Concurrent.TBCQueue
import Control.Monad
import Data.Fixed
import Data.Tacview (parseTime)
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import System.Clock.Seconds
import System.IO

newtype Args = Args {
    zipInput :: Maybe FilePath
}

parseArgs :: Parser Args
parseArgs = Args <$> parseZipIn where
    parseZipIn = optional . strArgument $ mconcat [
        help "ACMI to replay. Otherwise reads from stdin and writes to stdout",
        metavar "recording.zip.acmi"
        ]

main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    -- lol IO
    hSetBuffering stdout $ BlockBuffering Nothing
    hSetBuffering stdin $ BlockBuffering Nothing
    hSetBuffering stderr $ BlockBuffering Nothing

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Replays ACMI files at the rate they were recorded"
    parser >>= run

run :: Args -> IO ()
run Args{..} = do
    (src, _, _) <- Tacview.source zipInput
    void $ pipeline (newTBCQueueIO 1024) src delay

delay :: Channel c => c Text -> IO ()
delay source = void $ stateConsumeChannel source Nothing $ \ !mdelta l -> do
    nd <- if T.isPrefixOf "#" l
        then do
            hFlush stdout
            let t = parseTime l :: Double
            now <- getTime Monotonic
            case mdelta of
                -- If we don't have it yet,
                -- get the difference between now and the first timestamp.
                Nothing -> pure . Just $ realToFrac now - t
                -- Once we have it, apply that offset to the timestamp
                -- and sleep until then.
                Just d -> do
                    let sleepFor = (t + d) - realToFrac now :: Double
                    threadDelay . d2micro $ sleepFor
                    pure mdelta
        else pure mdelta
    T.putStrLn l
    pure nd

d2micro :: Double -> Int
d2micro d = fromIntegral m where
    MkFixed m = realToFrac d :: Micro
