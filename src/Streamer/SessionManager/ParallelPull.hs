module Streamer.SessionManager.ParallelPull
( startParallelPull
) where


import Streamer.Parents
import Streamer.Frames
import Streamer.HTTPClient  ( httpGetFrameBytes, constructFrameURL )
import Streamer.SessionManager.DigestsFile ( getDigestFileEntry )

import Control.Concurrent.MVar
import Control.Concurrent                       ( threadDelay, forkIO )
import Control.Monad                            ( forever )
import Control.Monad.STM                        ( atomically )
import System.Clock                             as K
import qualified Data.ByteString.Lazy           as L
import qualified Data.List.Ordered              as R
import qualified Control.Concurrent.Chan        as C
import qualified Control.Concurrent.STM.TChan   as STC
import qualified Control.Concurrent.BoundedChan as BCH


--------------------------------------------------------------------------------
-- Some constants

-- | Specifies how many threads are pulling in parallel. Each thread is assigned
-- a specific parent, from which it pulls frames.
parallelism :: Int
parallelism = 3


-- | Length of the channel that we use to transmit failed tasks.
failedTasksChanLength :: Int
failedTasksChanLength = 10


-- | Maximum number of failed tasks restarted at a time.
failedTasksFetchLimit :: Int
failedTasksFetchLimit = 5


-- | Local counter file, keeps the latest sequence number fetched
localCounterPath :: FilePath
localCounterPath = framesPersistPrefix ++ "counter"


--------------------------------------------------------------------------------
-- Data Types

data PullTask = PullTask
    { ptParentIp     :: String
    , ptParentPort   :: Int
    , ptFrameSeqNr   :: Int
    , ptFrameDigest  :: String
    , ptStatus       :: Maybe Bool    -- consider removing this record field
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Functions

--------------------------------------------------------------------------------
-- | Handles pulling of frames in parallel, using multiple threads.
-- Each thread is assigned a parent, and from each parent we try to pull several
-- frames (i.e., PullTask) sequentially.
startParallelPull ::
    [Parent]                       -- list of all parents
    -> BCH.BoundedChan (String)    -- digests channel
    -> [Int]                       -- frames consumed so far
    -> IO ()
startParallelPull parents digestsChan _ = do
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
    -> [C.Chan (PullTask)]          -- pull threads input channel
    -> IO ()
assignPullTasks parents digChan fTChan pTIChan =
    forever $ do
        -- Get the failed tasks.
        fTasks <- fetchFailedTasks fTChan failedTasksFetchLimit []
        -- Ignore the parents corresponding to failed tasks.
        if length fTasks > 0
            then putStrLn $ "[pulltask] Found failed tasks: " ++ (show fTasks)
            else putStr ""
        ignoreParents parents $
            map (\t -> (ptParentIp t, ptParentPort t)) fTasks
        -- Fetch any new frame (tuple) from the digest file.
        let failedTups = getTuples fTasks
        t <- getDigestFileEntry digChan []
        case t of
            Just tup  -> getParentsAndAssign parents (tup:failedTups) pTIChan
            Nothing   -> getParentsAndAssign parents failedTups pTIChan
        where
            -- Transforms failed PullTask into tuples of (SeqNr, Digest).
            getTuples ftu = map (\t -> (ptFrameSeqNr t, ptFrameDigest t)) ftu


--------------------------------------------------------------------------------
-- | Searches the top preferable parents and then assigns the tuples to these
-- parents. Blocks if no viable parent (i.e., with ignore == False) is found.
getParentsAndAssign ::
    [Parent]                -- all parents (including ignored ones)
    -> [(Int, String)]      -- tuples to be assigned
    -> [C.Chan (PullTask)]  -- pull threads input channel
    -> IO ()
getParentsAndAssign parents tuples channels = do
    tParents <- getTopKParents parents parallelism []
    if null tParents
        then do
            threadDelay 500000
            putStrLn $ "[pulltask] No parents found!"
            getParentsAndAssign parents tuples channels
        else do
            assignTuples tuples tParents channels 1
            threadDelay 300000
            return ()


--------------------------------------------------------------------------------
-- | Creates PullTasks out of tuples, then does a round-robin assignments of
-- these tasks to parents.
assignTuples ::
    [(Int, String)]         -- tuples that will be assigned
    -> [(String, Int, Int)] -- parents
    -> [C.Chan (PullTask)]  -- pull threads input channel
    -> Int                  -- counter to help distribute the tuples evenly
    -> IO ()
assignTuples [] _ _ _ = do
    -- putStrLn "Finished assigning tuples."
    return ()
assignTuples _ [] _ _ = do
    putStrLn "[pulltask] No parents available for assignTuples!"
assignTuples _ _ [] _ = do
    putStrLn "[pulltask] No thread channels available for assignTuples!"
assignTuples ((tSeq, tDig):xt) parents channels cnt = do
        putStrLn $ "[pulltask] Assigning (" ++ (show tSeq) ++ ", " ++ (show tDig) ++
            ") to parent (" ++ (show parIp) ++ ", " ++ (show parPort) ++
            ") and thread " ++ (show idxChannels)
        C.writeChan chan $ PullTask { ptParentIp = parIp
                                    , ptParentPort = parPort
                                    , ptFrameSeqNr = tSeq
                                    , ptFrameDigest = tDig
                                    , ptStatus = Nothing}
        assignTuples xt parents channels $ cnt+1
    where
        idxParents = (length parents) `rem` cnt
        idxChannels = (length channels) `rem` cnt
        (parIp, parPort, _) = parents!!idxParents
        chan = channels!!idxChannels


--------------------------------------------------------------------------------
-- | Collects the tasks finished by PullThreads and writes them to the channel
-- dedicated for these tasks.
-- These tasks are later consumed by assignPullTasks.
collectPullTasks ::
    BCH.BoundedChan (PullTask)      -- failed tasks
    -> [STC.TChan (PullTask)]       -- output of pull threads
    -> IO ()
collectPullTasks failedTasksChan pullThreadsOChan =
    forever $ do
        mapM_ (extractTask failedTasksChan) pullThreadsOChan
        threadDelay 100000
    where
        extractTask fChan oChan = do
            empt <- atomically $ STC.isEmptyTChan oChan
            if empt
                then return ()
                else do
                    a <- atomically $ STC.readTChan oChan
                    BCH.writeChan fChan a
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
        parentToTuple pa = do
            cnt <- readMVar $ pLatestCounter pa
            return (pIp pa, pPort pa, cnt)
        -- The accumulator list is ordered by the counter value.
        compareByCounter (_, _, cnt1) (_, _, cnt2) =
            compare cnt1 cnt2


--------------------------------------------------------------------------------
-- | Creates k threads, each of them having an input and an output channel.
-- The threads receive PullTasks from their input channel, execute these tasks
-- by pulling the associated frame from the given parent, and return a status
-- in the output channel.
-- The status of an executed PullTask is marked in ptStatus record, either True
-- if success, or False if failure.
-- The parameter k represents the degree of parallelism.
sparkPullThreads ::
    Int         -- how many threads should start
    -> IO ([C.Chan (PullTask)], [STC.TChan (PullTask)])
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
                outChan <- STC.newTChanIO :: IO (STC.TChan PullTask)
                _ <- forkIO (pullThreadFunc inChan outChan)
                return (inChan, outChan)
            foldChans (inCs, outCs) (ic, oc) =
                (ic:inCs, oc:outCs)


--------------------------------------------------------------------------------
-- | Main function executed by every thread that pulls frames.
pullThreadFunc ::
    C.Chan PullTask         -- input channel
    -> STC.TChan PullTask   -- output channel
    -> IO ()
pullThreadFunc iC oC =
    forever $ do
        C.readChan iC
        >>= (\task -> executePullTask task
        >>= (\mSeqNr -> case mSeqNr of
                            Just seqNr ->
                                deliverSeqNr seqNr >> updateLocalCounter seqNr
                            Nothing    ->
                                atomically $ STC.writeTChan oC (fdTask task)
                                >> return ()
            ))
    where
        -- Transforms a Task into a failed Task, i.e., updates the ptStatus
        -- record field to False.
        fdTask task = task { ptStatus = Just False }
        -- Pretty prints a K.TimeSpec data.
        showTime t = (show $ K.sec t) ++ "." ++ (show $ K.nsec t)
        -- Delivers (prints) a seq. number, tagged with the current real time.
        deliverSeqNr s = K.getTime (K.Realtime)
            >>= (\t -> putStrLn $ (showTime t) ++ " d " ++ (show s))
        -- Writes the sequnce number to the local counter file
        updateLocalCounter s =
            writeFile localCounterPath $ show s


--------------------------------------------------------------------------------
-- | Pulls a frame from a given parent and returns the sequence number for that
-- frame. The frame is identified by a (sequence number, digest) tuple.
executePullTask ::
    PullTask
    -> IO (Maybe Int)
executePullTask task = do
    -- The port that serves frames is predefined: parentListeningPort = 80
    bytes <- pullBytes (ptParentIp task) parentListeningPort seqNr
    putStrLn $ "Verifying if the digest matches"
    if verifyDigest (ptFrameDigest task) bytes == True
        then persistFrame seqNr bytes >> return (Just seqNr)
        else putStrLn "Invalid digest!" >> return Nothing
    where
        seqNr = ptFrameSeqNr task


--------------------------------------------------------------------------------
-- | Pulls a frame from a given parent. Returns a Lazy Bytestring containing the
-- data that was pulled. In case of error, the Bytestring is empty. Various
-- reasons can cause errors: the parent contains corrupted frames, it has no
-- frames at all, has no running http server, etc.
pullBytes ::
    String  -- IP of the parent
    -> Int  -- port of the parent
    -> Int  -- sequence number of the frame that will be pulled
    -> IO (L.ByteString)
pullBytes ip port seqNr = do
    result <- httpGetFrameBytes $ constructFrameURL ip port seqNr
    case result of
        Just bytes  -> return bytes
        Nothing     -> return L.empty


--------------------------------------------------------------------------------
-- | Reads from the channel with failed tasks and returns at most P of them.
-- Does not block: if less than P tasks are available, it only returns those
-- tasks.
fetchFailedTasks ::
    BCH.BoundedChan (PullTask)  -- channel with failed PullTasks
    -> Int                      -- limit on how many tasks shall be retrieved
    -> [PullTask]               -- accumulator
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
ignoreParents ::
    [Parent]            -- all parents
    -> [(String, Int)]  -- identifiers of parents that will be marked with
                        -- ignore = True
    -> IO ()
ignoreParents _       []              = return ()
ignoreParents parents ((ip, port):xs) = do
    putStrLn $ "Warning: Ignoring parent: " ++ ip ++ ":" ++ (show port)
    mapM_ (\p -> if (pIp p == ip) && (pPort p == port)
                then swapMVar (pIgnore p) True >> return ()
                else return ()) parents
    ignoreParents parents xs