module Streamer.SessionManager where

import Streamer.Types
import Streamer.PullNodes
import Streamer.Util (maybeRead)

import System.IO ( Handle
                 , IOMode(ReadMode)
                 , openFile
                 , hGetLine
                 , hIsEOF
                 )
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent (forkIO)
import Data.Maybe


-- Path to the file holding the list of digests
digestsFilePath :: FilePath
digestsFilePath = "/mnt/lpd-distlib/streamer/v1/digests.list"


-- How much to wait if the digests file EOF found, in microseconds
consumeDigestsFileDelay :: Int
consumeDigestsFileDelay = 1000000


-- Length of the BoundedChan which holds the digests
digestsChanLength :: Int
digestsChanLength = 5


data SessionManager = SessionManager
    { smPullNodes   :: PullNodesList
    , smFrames      :: [Frame]
    }  deriving (Eq)


-- SessionManager is an instance of Show typeclass, and we want a simplified
-- string representation.
instance Show SessionManager where
    show sm = "{smFrames = " ++ (show $ smFrames sm) ++ " }"


-- | Main function executed by each SessionManager.
startSessionManager :: SessionManager -> IO ()
startSessionManager sManager = do
    h    <- openFile digestsFilePath ReadMode
    chan <- newBoundedChan digestsChanLength
    _    <- forkIO (consumeDgstFile h chan 0)
    forever $ do
        digestLine <- readChan chan
        putStrLn $ "Pulling a frame using meta " ++ (show digestLine)
        let b = pullFrame sManager digestLine
        putStrLn $ "Status : " ++ (show b)


pullFrame :: SessionManager -> String -> Bool
pullFrame sManager digestLine
        | isJust maybeFmTuple =
            doPullOneFrame sManager $ fromJust maybeFmTuple
        | otherwise = False
        where
            maybeFmTuple = digestLineToFrameMetadata digestLine


-- | Takes a line from the digests file and parses it, yielding a sequence
-- number and the associated digest.
--
-- For example,
-- >    digestLineToFrameMetadata
--          "2 076a27c79e5ace2a3d47f9dd2e83e4ff6ea8872b3c2218f66c92b89b55f36560"
-- will output the following tuple:
-- > ("2", "076a27c79e5ace2a3d47f9dd2e83e4ff6ea8872b3c2218f66c92b89b55f36560")
digestLineToFrameMetadata :: String -> Maybe (Int, String)
digestLineToFrameMetadata "" = Nothing
digestLineToFrameMetadata line
    | (length items == 2) && (isJust seqNr) && (length digest == 64) =
        Just (fromJust seqNr, digest)
    | otherwise = Nothing
    where
        items = words line
        seqNr = maybeRead $ items!!0 :: Maybe Int
        digest = items!!1


-- | Pulls a frame from the active node. Returns only when it cannot pull the
-- frame from the active node. Various reasons can account for this: the active
-- node contains corrupted frames, it has no frames at all, it is too slow, etc.
--
-- This function returns a string indicating the reason why it stopped pulling
-- frames from the active node.
doPullOneFrame :: SessionManager -> (Int, String) -> Bool
doPullOneFrame sManager (seqNr, digest) =
    -- putStrLn $ "Pulling frame " ++ seqNr ++ " with digest " ++ digest
    True


-- | Consumes lines from a digest file and appends the lines to a BoundedChan.
-- Takes as arguments a file handle and a BoundedChan and a number. The third
-- parameter indicates how many lines to initially skip before starting to
-- append to the BoundedChan.
--
-- This function never returns (it will continue to call itself recursively),
-- therefore it should execute in its own thread.
--
-- Example:
-- >    handle  <- openFile "/tmp/a" ReadMode
-- >    chan    <- newBoundedChan 10
-- >    forkIO (consumeDgstFile handle chan)
--
-- Note: Apparently, BoundedChan is not exception safe (i.e. we can't do
-- killThread on the thread executing consumeDgstFile). For an exception safe
-- chan: Control.Concurrent.Chan (http://stackoverflow.com/a/9250200/919383)
consumeDgstFile :: Handle -> BoundedChan (String) -> Int -> IO ()
consumeDgstFile h c skipNr = do
    isEof <- hIsEOF h
    if isEof == True
        then threadDelay consumeDigestsFileDelay
        else do
            line <- hGetLine h
            if skipNr > 0
                then consumeDgstFile h c $ skipNr - 1
                else writeChan c line
    consumeDgstFile h c 0