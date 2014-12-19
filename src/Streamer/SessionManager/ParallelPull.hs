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
import Control.Concurrent                       ( threadDelay, forkIO )
import Control.Monad                            ( forever )
import qualified Data.ByteString.Lazy           as L
import qualified Data.List.Ordered              as R
import qualified Data.Sequence                  as S
import qualified Control.Concurrent.Chan        as C
import qualified Control.Concurrent.BoundedChan as BCH


--------------------------------------------------------------------------------
-- Some constants

-- | Specifies how many threads are pulling in parallel. Each thread is assigned
-- a specific parent, from which it pulls frames.
parallelism :: Int
parallelism = 3


-- | Specifies how many frames to pull sequentially from a parent.
parentLoad :: Int
parentLoad = 3

-- | Length of the channel that we use to transmit failed tasks.
failedTasksChanLength :: Int
failedTasksChanLength = 10


-- | Maximum number of failed tasks restarted at a time.
failedTasksFetchLimit :: Int
failedTasksFetchLimit = 5


--------------------------------------------------------------------------------
-- Data Types

data PullTask = PullTask
    { tParentIp     :: String
    , tParentPort   :: Int
    , tFrameSeqNr   :: Int
    , tFrameDigest  :: String
    , tStatus       :: Maybe Bool    -- consider removing this record field
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Functions

--------------------------------------------------------------------------------
-- | Handles pulling of frames in parallel, using multiple threads.
-- Each thread is assigned a parent, and from each parent we try to pull several
-- frames sequentially.
startParallelPull :: [Parent] -> BCH.BoundedChan (String) -> [Int] -> IO ()
startParallelPull parents digestsChan sNrSoFar = do
    (inChans, outChans) <- sparkPullThreads parallelism
    failedTasksChan     <- BCH.newBoundedChan failedTasksChanLength
    _ <- forkIO (assignPullTasks parents digestsChan failedTasksChan inChans)
    collectPullTasks failedTasksChan outChans


--------------------------------------------------------------------------------
-- | Creates PullTasks and assigns them to a specific PullThread by sending it
-- on the input channel of that PullThread.
assignPullTasks ::
    [Parent]                        -- parent to which we can assign tasks
    -> BCH.BoundedChan (String)     -- digests for tasks come through here
    -> BCH.BoundedChan (PullTask)   -- failed tasks come through here
    -> [C.Chan (PullTask)]          -- input to pull threads
    -> IO ()
assignPullTasks parents digChan fTChan pTIChan =
    forever $ do
        fTasks <- fetchFailedTasks fTChan failedTasksFetchLimit []
        ignoreParents parents $ map (\t -> (tParentIp t, tParentPort t)) fTasks
        -- Naive: assign to each parent as much as it can take..
        -- TODO!
        return ()
        where
            -- Obtains the top K parents sorted descending by their counters
            tParents = getTopKParents parents parallelism
            -- Transforms failed Tasks into tuples of (SeqNr, Digest)
            fFrames fT = map (\t -> (tFrameSeqNr t, tFrameDigest t)) fT


--------------------------------------------------------------------------------
-- | Collects the tasks finished by PullThreads and writes them to the channel
-- dedicated for these tasks.
-- These tasks are later consumed by assignPullTasks.
collectPullTasks ::
    BCH.BoundedChan (PullTask)      -- failed tasks
    -> [C.Chan (PullTask)]          -- output of pull threads
    -> IO ()
collectPullTasks failedTasksChan pullThreadsOChan = do
    return ()


--------------------------------------------------------------------------------
-- | Computes the first K parents, in descending order of their counter record.
-- Returns a list where the top K parents are identified by tuples of the form
-- (pIp, pPort, pLatestCounter).
getTopKParents ::
    [Parent]                        -- initial list of parents
    -> Int                          -- number of parents to return
    -> [(String, Int, Int)]         -- accumulator
    -> IO [(String, Int, Int)]
getTopKParents [] k accum = return $ take k accum
getTopKParents (p:xp) k accum = do
    ignore <- readMVar $ pIgnore p
    -- First, we check the Ignore flag.
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


--------------------------------------------------------------------------------
-- | Creates k threads, each of them having an input and an output channel.
-- The threads receive PullTasks from their input channel, execute these tasks
-- by pulling the associated frame from the given parent, and return a status
-- in the output channel.
-- The status of an executed PullTask is marked in tStatus record, either True
-- if success, or False if failure.
-- The parameter k represents the degree of parallelism.
sparkPullThreads :: Int -> IO ([C.Chan (PullTask)], [C.Chan (PullTask)])
sparkPullThreads para = do
    if para < 1
        then return ([],[])
        else do
            chanz <- mapM sparkPullThread [1..para]
            return $ foldl foldChans ([],[]) chanz
        where
            -- This returns a tuple with the input and output channels.
            sparkPullThread _ = do
                inChan <- C.newChan :: IO (C.Chan PullTask)
                outChan <- C.newChan :: IO (C.Chan PullTask)
                _ <- forkIO (pullThreadFunc inChan outChan)
                return (inChan, outChan)
            foldChans (inCs, outCs) (ic, oc) =
                (ic:inCs, oc:outCs)


--------------------------------------------------------------------------------
-- | Main function executed by every thread that pulls frames.
pullThreadFunc :: C.Chan PullTask -> C.Chan PullTask -> IO ()
pullThreadFunc iC oC = forever $ do
    C.readChan iC
    >>= (\task -> executePullTask task
    >>= (\mSeqNr -> case mSeqNr of
                        Just seqNr -> putStrLn $ "Finished.." ++ (show seqNr)
                        Nothing    -> C.writeChan oC (fdTask task) >> return ()
        ))
    where
        -- Transforms a Task into a failed Task, i.e., updates the tStatus
        -- record field to False.
        fdTask task = task { tStatus = Just False }


--------------------------------------------------------------------------------
-- | Pulls a frame from a given parent and returns the sequence number for that
-- frame. The frame is identified by a (sequence number, digest) tuple.
executePullTask :: PullTask -> IO (Maybe Int)
executePullTask task = do
    bytes <- pullBytes (tParentIp task) (tParentPort task) seqNr
    putStrLn $ "Verifying if the digest matches"
    if verifyDigest (tFrameDigest task) bytes == True
        then do
            persistFrame seqNr bytes
            return $ Just seqNr
        else do
            putStrLn "Invalid digest!"
            return Nothing
    where
        seqNr = tFrameSeqNr task


--------------------------------------------------------------------------------
-- | Pulls a frame from a given parent. Returns a Lazy Bytestring containing the
-- data that was pulled. In case of error, the Bytestring is empty. Various
-- reasons can cause errors: the parent contains corrupted frames, it has no
-- frames at all, has no running http server, etc.
pullBytes :: String -> Int -> Int -> IO (L.ByteString)
pullBytes ip port seqNr = do
    result <- httpGetFrameBytes $ constructFrameURL ip port seqNr
    case result of
        Just bytes  -> return bytes
        Nothing     -> return L.empty


--------------------------------------------------------------------------------
-- | Reads the next entry from the digest file. An entry has the form of a tuple
-- (sequence number, digest).
-- The digest file is represented as a fifo queue (Bounded Chan).
-- The second parameter is a list of Int representing sequence numbers that this
-- function shall skip (if encountered in the fifo queue).
-- This function block if there is no entry available.
getDigestFileEntry ::
    BCH.BoundedChan (String)
    -> [Int]
    -> IO (Maybe (Int, String))
getDigestFileEntry chan skipSeqNrList = do
    mLine <- BCH.tryReadChan chan
    case mLine of
        Just digestLine -> do
            let maybeFmTuple = digestLineToFrameMetadata digestLine
            case maybeFmTuple of
                Just (seqNr, digest) -> do
                    if R.member seqNr skipSeqNrList
                        then getDigestFileEntry chan skipSeqNrList
                        else return $ Just (seqNr, digest)
                Nothing -> getDigestFileEntry chan skipSeqNrList
        Nothing -> return Nothing


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- | Reads from the channel with failed tasks and returns at most P of them.
-- Does not block: if less than P tasks are available, it only returns those
-- tasks.
fetchFailedTasks :: BCH.BoundedChan (PullTask)
                  -> Int
                  -> [PullTask]
                  -> IO ([PullTask])
fetchFailedTasks _      0 accum = return accum
fetchFailedTasks ffChan l accum = do
    mFF <- BCH.tryReadChan ffChan
    case mFF of
        Just fFrame -> fetchFailedTasks ffChan (l-1) (fFrame:accum)
        Nothing     -> return accum


--------------------------------------------------------------------------------
-- | Marks the pIgnore record field for some given parents to True.
-- The parents that will be marked are identified by the second argument: a
-- tuple of (Ip, Port).
ignoreParents :: [Parent] -> [(String, Int)] -> IO ()
ignoreParents _       []              = return ()
ignoreParents parents ((ip, port):xs) = do
    putStrLn $ "Warning: Ignoring parent: " ++ ip ++ ":" ++ (show port)
    mapM_ (\p -> if (pIp p == ip) && (pPort p == port)
                then swapMVar (pIgnore p) True >> return ()
                else return ()) parents
    ignoreParents parents xs