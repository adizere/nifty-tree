module Streamer.SessionManager where


import Streamer.Frame
import Streamer.PullNodes
import Streamer.Util        (maybeRead)
import Streamer.HTTPClient  (doRequest, constructURL)

import System.IO ( Handle
                 , IOMode(ReadMode)
                 , openFile
                 , hGetLine
                 , hIsEOF
                 )
import Control.Concurrent (threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent (forkIO)
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.List.Ordered    as R


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
    , smFramesSeqNr :: [Int]
    }  deriving (Eq)


-- SessionManager is an instance of Show typeclass, and we want a simplified
-- string representation.
instance Show SessionManager where
    show sm = "{smFramesSeqNr = " ++ (show $ smFramesSeqNr sm) ++ " }"


-- | Main function executed by each SessionManager.
startSessionManager :: SessionManager -> IO ()
startSessionManager sManager = do
    h    <- openFile digestsFilePath ReadMode
    chan <- newBoundedChan digestsChanLength
    _    <- forkIO (consumeDgstFile h chan 0)
    getFrames (pnlActiveNode $ smPullNodes sManager)
              chan
              (smFramesSeqNr sManager)


getFrames :: PullNode -> BoundedChan (String) -> [Int] -> IO ()
getFrames activeNode chan sNrSoFar = do
    (seqNr, digest) <- getDigestFileEntry chan sNrSoFar
    putStrLn $ "These are the frames so far.. " ++ (show sNrSoFar)
    putStrLn $ "Pulling a frame using meta " ++ (show seqNr) ++ " " ++ (digest)
    mFrameSeqNr <- pullFrame activeNode (seqNr, digest)
    case mFrameSeqNr of
        Just sNr -> getFrames activeNode chan $ R.insertSet sNr sNrSoFar
        Nothing  -> getFrames activeNode chan sNrSoFar


-- | Pulls a frame from a given node and returns the sequence number for that
-- frame. The frame is identified by a (sequence number, digest) tuple encoded
-- in a String.
pullFrame :: PullNode -> (Int, String) -> IO (Maybe Int)
pullFrame node (seqNr, digest) = do
    bytes <- pullBytes node seqNr
    putStrLn $ "Verifying if the digest matches"
    if verifyDigest digest bytes == True
        then do
            persistFrame seqNr bytes
            return $ Just seqNr
        else do
            putStrLn "Invalid digest!"
            return Nothing


-- | Pulls a frame from a given node. Returns a Lazy Bytestring containing the
-- data that was pulled. In case of error, the Bytestring is empty.
-- Various reasons can cause errors: the node contains corrupted frames, it has
-- no frames at all, has no running http server, etc.
pullBytes :: PullNode -> Int -> IO (L.ByteString)
pullBytes node seqNr = do
    result <- doRequest $ constructURL node seqNr
    case result of
        Just bytes  -> return bytes
        Nothing     -> return L.empty


getDigestFileEntry :: BoundedChan (String) -> [Int] -> IO (Int, String)
getDigestFileEntry chan skipSeqNrList = do
    digestLine <- readChan chan
    let maybeFmTuple = digestLineToFrameMetadata digestLine
    case maybeFmTuple of
        Just (seqNr, digest) -> do
            if R.member seqNr skipSeqNrList
                then getDigestFileEntry chan skipSeqNrList
                else return (seqNr, digest)
        Nothing -> getDigestFileEntry chan skipSeqNrList


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