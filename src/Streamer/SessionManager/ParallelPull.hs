module Streamer.SessionManager.ParallelPull
( startParallelPull
) where


import Streamer.Parents
import Streamer.Frames
import Streamer.HTTPClient  ( httpGetFrameBytes, constructFrameURL )
import Streamer.Util        ( maybeRead )

import Data.Maybe
import Control.Concurrent.MVar
import Data.Ord
import Control.Concurrent               ( threadDelay, forkIO )
import qualified Data.ByteString.Lazy   as L
import qualified Data.List.Ordered      as R
import qualified Data.Sequence          as S
import qualified Control.Concurrent.Chan        as C
import qualified Control.Concurrent.BoundedChan as BCH


-- | Specifies how many threads are pulling in parallel. Each thread is assigned
-- a specific parent, from which it pulls frames.
parallelism :: Int
parallelism = 3


-- | Specifies how many frames to pull sequentially from a parent.
parentLoad :: Int
parentLoad = 3


failedTasksChanLength :: Int
failedTasksChanLength = 10


data PullTask = PullTask
    { tParentIp     :: String
    , tParentPort   :: Int
    , tFrameSeqNr   :: Int
    , tFrameDigest  :: String
    , tStatus       :: Maybe Bool
    } deriving (Eq, Show)


-- | Handles pulling of frames in parallel, using multiple threads.
-- Each thread is assigned a parent, and from each parent we try to pull several
-- frames sequentially.
startParallelPull :: [Parent] -> BCH.BoundedChan (String) -> [Int] -> IO ()
startParallelPull parents digestsChan sNrSoFar = do
    (inChans, outChans) <- sparkPullThreads parallelism
    return ()


assignTasksToPullThreads :: BCH.BoundedChan (String)        -- digests come through
                            -> BCH.BoundedChan (PullTask)   -- failed tasks
                            -> [C.Chan (PullTask)]          -- input to pull threads
                            -> IO ()
assignTasksToPullThreads digestsChan failedTasksChan pullThreadsIChan = do
    return ()


collectFinishedTasks :: BCH.BoundedChan (PullTask)      -- failed tasks
                        -> [C.Chan (PullTask)] -- output of pull threads
                        -> IO ()
collectFinishedTasks failedTasksChan pullThreadsOChan = do
    return ()


getTopKParents :: [Parent]                  -- initial list of parents
                  -> Int                    -- number of parents to return
                  -> [(String, Int, Int)]   -- accumulator
                  -> IO [(String, Int, Int)]
getTopKParents [] k accum = return $ take k accum
getTopKParents (p:xp) k accum = do
    ignore <- readMVar $ pIgnore p
    -- First, we check the Ignore flag..
    if ignore == True
        then getTopKParents xp k accum
        -- If the parent is not ignored, insert it in the accumulator list.
        else do
            tup <- parentToTuple p
            getTopKParents xp k $ R.insertSetBy compareByCounter tup accum
    where
        parentToTuple p = do
            cnt <- readMVar $ pLatestCounter p
            return (pIp p, pPort p, cnt)
        -- The accumulator list is ordered by the counter value.
        compareByCounter (_, _, cnt1) (_, _, cnt2) =
            compare cnt1 cnt2


sparkPullThreads :: Int
                    -> IO ([C.Chan (PullTask)], [C.Chan (PullTask)])
sparkPullThreads parallelism = do
    if parallelism < 1
        then return ([],[])
        else return ([], []) -- mapM sparkPullThread [1..parallelism]
        where
            sparkPullThread _ = do
                inChan <- C.newChan :: IO (C.Chan PullTask)
                outChan <- C.newChan :: IO (C.Chan PullTask)
                forkIO (pullThreadFunc inChan outChan)

pullThreadFunc :: C.Chan PullTask -> C.Chan PullTask -> IO ()
pullThreadFunc _ _ = do
    return ()



-- | Pulls a frame from a given parent and returns the sequence number for that
-- frame. The frame is identified by a (sequence number, digest) tuple.
pullFrame :: Parent -> (Int, String) -> IO (Maybe Int)
pullFrame prnt (seqNr, digest) = do
    bytes <- pullBytes prnt seqNr
    putStrLn $ "Verifying if the digest matches"
    if verifyDigest digest bytes == True
        then do
            persistFrame seqNr bytes
            return $ Just seqNr
        else do
            putStrLn "Invalid digest!"
            return Nothing


-- | Pulls a frame from a given parent. Returns a Lazy Bytestring containing the
-- data that was pulled. In case of error, the Bytestring is empty. Various
-- reasons can cause errors: the parent contains corrupted frames, it has no
-- frames at all, has no running http server, etc.
pullBytes :: Parent -> Int -> IO (L.ByteString)
pullBytes p seqNr = do
    result <- httpGetFrameBytes $ constructFrameURL (pIp p) (pPort p) seqNr
    case result of
        Just bytes  -> return bytes
        Nothing     -> return L.empty


-- | Reads the next entry from the digest file. An entry has the form of a tuple
-- (sequence number, digest).
-- The digest file is represented as a fifo queue (Bounded Chan).
-- The second parameter is a list of Int representing sequence numbers that this
-- function shall skip (if encountered in the fifo queue).
getDigestFileEntry :: BCH.BoundedChan (String) -> [Int] -> IO (Int, String)
getDigestFileEntry chan skipSeqNrList = do
    digestLine <- BCH.readChan chan
    let maybeFmTuple = digestLineToFrameMetadata digestLine
    case maybeFmTuple of
        Just (seqNr, digest) -> do
            if R.member seqNr skipSeqNrList
                then getDigestFileEntry chan skipSeqNrList
                else return (seqNr, digest)
        Nothing -> getDigestFileEntry chan skipSeqNrList


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