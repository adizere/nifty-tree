module Streamer.SessionManager.ParallelPull
( startParallelPull
) where


import Streamer.Parents
import Streamer.Frames
import Streamer.HTTPClient  ( httpGetFrameBytes, constructFrameURL )
import Streamer.SessionManager.DigestsFile ( collectDigestFileEntries )

import Control.Concurrent.MVar
import System.IO                                ( withFile
                                                , IOMode(WriteMode)
                                                , hPutStr )
import Control.Concurrent                       ( threadDelay, forkIO )
import Control.Monad                            ( forever )
import Control.Monad.STM                        ( atomically )
import Data.Maybe                               ( isJust, fromJust )
import System.Clock                             as K
import qualified Data.ByteString.Lazy           as L
import qualified Data.List.Ordered              as R
import qualified Control.Concurrent.Chan        as C
import qualified Control.Concurrent.STM.TChan   as STC
import qualified Control.Concurrent.STM.TQueue  as STQ
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


-- | Retry delay, when we get a 404
retryDelay :: Int
retryDelay = 300000


-- | Retry limit: how many time try to pull bytes in case of HTTP server error
retryLimit :: Int
retryLimit = 3

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
    -> STQ.TQueue (String)         -- digests channel
    -> [Int]                       -- frames consumed so far
    -> IO ()
startParallelPull parents digQu _ = do
    (inChans, outChans) <- sparkPullThreads parallelism
    failedTasksChan     <- BCH.newBoundedChan failedTasksChanLength
    _ <- forkIO (assignPullTasks parents digQu failedTasksChan inChans [])
    collectPullTasks failedTasksChan outChans 0


--------------------------------------------------------------------------------
-- | Creates PullTasks and assigns them to a specific PullThread by sending it
-- on the input channel of that PullThread.
assignPullTasks ::
    [Parent]                        -- parent to which we can assign tasks
    -> STQ.TQueue (String)          -- digests for tasks come through here
    -> BCH.BoundedChan (PullTask)   -- failed tasks come through here
    -> [C.Chan (PullTask)]          -- pull threads input channel
    -> [(Int, String)]              -- unassigned tasks (in the form of tuples)
    -> IO ()
assignPullTasks prntz digQu fTChan pTIChan unag = do
        -- Get the failed tasks.
        fTasks <- fetchFailedTasks fTChan failedTasksFetchLimit []
        -- Ignore the parents corresponding to failed tasks.
        if length fTasks > 0
            then putStrLn $ "[pulltask] Found failed tasks: " ++ (show fTasks)
            else putStr ""
        ignoreParents prntz $
            map (\t -> (ptParentIp t, ptParentPort t)) fTasks
        -- Fetch any new frame (tuple) from the digest file.
        let faildTups = getTuples fTasks
        ntriz <- collectDigestFileEntries digQu 10 []
        newUnag <- getParentsAndAssign prntz (unag++ntriz++faildTups) pTIChan
        if (null fTasks) && (null ntriz) && (null newUnag)
            then
            -- If there were no tasks, then we can rest a bit
                threadDelay 300000
                >> assignPullTasks prntz digQu fTChan pTIChan newUnag
            else assignPullTasks prntz digQu fTChan pTIChan newUnag
        where
            -- Transforms failed PullTask into tuples of (SeqNr, Digest).
            getTuples ftu = map (\t -> (ptFrameSeqNr t, ptFrameDigest t)) ftu


--------------------------------------------------------------------------------
-- | Searches the top preferable parents and then assigns the tuples to these
-- parents. Blocks if no viable parent (i.e., with ignore == False) is found.
-- If any tuples remain unassigned, it returns them.
getParentsAndAssign ::
    [Parent]                -- all parents (including ignored ones)
    -> [(Int, String)]      -- tuples to be assigned
    -> [C.Chan (PullTask)]  -- pull threads input channel
    -> IO ([(Int, String)])
getParentsAndAssign parents tp chnlz = do
    topParents <- getTopKParents parents parallelism []
    -- putStrLn $ "-- Using parents:" ++ (show topParents) ++ "-- for tuples:"
    --         ++ (show tp) ++ "--"
    if null topParents
        then do
            threadDelay 500000
            putStrLn $ "[pulltask] No parents found!"
            getParentsAndAssign parents tp chnlz
        else
            assignTuples tp topParents chnlz 1 [] >>= (\unag -> return unag)


--------------------------------------------------------------------------------
-- | Creates PullTasks out of tuples, then does a round-robin assignments of
-- these tasks to parents. If any tasks remain un-assigned, this function will
-- return them.
assignTuples ::
    [(Int, String)]         -- tuples that will be assigned
    -> [(String, Int, Int)] -- parents (pIp, pPort, pLatestCounter)
    -> [C.Chan (PullTask)]  -- pull threads input channel
    -> Int                  -- counter to help distribute the tuples evenly
    -> [(Int, String)]      -- accumulator to save the unassigned tuples
    -> IO ([(Int, String)])
assignTuples [] _ _ _ unag = do
    return unag
assignTuples _ [] _ _ unag = do
    putStrLn "[pulltask] No parents available for assignTuples!"
    >> return unag
assignTuples _ _ [] _ unag = do
    putStrLn "[pulltask] No thread channels available for assignTuples!"
    >> return unag
assignTuples ((tSeq, tDig):xt) prntz chnlz cnt unag = do
        -- We assign the task to a random node if the counter is >= seq number;
        -- Otherwise: if the counter of the top parent is >= seq number, then
        -- assign to the top parent, else save the task and assign it later.
        if rndCntr >= tSeq
            then
                assignToParent rndIp rndPort rndCntr
                >> assignTuples xt prntz chnlz (cnt+1) unag
            else if topCntr >= tSeq
                then
                    assignToParent topIp topPort topCntr
                    >> assignTuples xt prntz chnlz (cnt+1) unag
                else assignTuples xt prntz chnlz (cnt+1) $ unag++[(tSeq, tDig)]
    where
        idxParents = cnt `mod` (length prntz)
        idxChannels = cnt `mod` (length chnlz)
        chan = chnlz !! idxChannels
        -- A random parent
        (rndIp, rndPort, rndCntr) = prntz !! idxParents
        -- The top parent, having the highest counter
        (topIp, topPort, topCntr) = head prntz
        -- Assigns a tuple to a given parent (ip, port)
        assignToParent ip port leCntr = do
            C.writeChan chan $ PullTask { ptParentIp = ip
                                        , ptParentPort = port
                                        , ptFrameSeqNr = tSeq
                                        , ptFrameDigest = tDig
                                        , ptStatus = Nothing}
            putStrLn $ "[pulltask] Assigning (" ++ (show tSeq) ++ ", " ++
                    (show tDig) ++ ") to parent (" ++ (show ip) ++ ", " ++
                    (show port) ++ ", " ++ (show leCntr) ++ ") and thread " ++
                    (show idxChannels)


--------------------------------------------------------------------------------
-- | Collects the tasks finished by PullThreads and writes them to the channel
-- dedicated for these tasks.
-- These tasks are later consumed by assignPullTasks.
collectPullTasks ::
    BCH.BoundedChan (PullTask)      -- failed tasks
    -> [STC.TChan (PullTask)]       -- output of pull threads
    -> Int                          -- max sequence number seen so far
    -> IO ()
collectPullTasks fC pC mS = do
        readySeqNr <- mapM (extractTask fC) pC
        let (should, newMS) = shouldUpdateCounter readySeqNr mS
        if (should == True)
            then updLocalCntr newMS >> collectPullTasks fC pC newMS
            else threadDelay 100000 >> collectPullTasks fC pC mS
    where
        -- Writes the sequnce number to the local counter file
        updLocalCntr s =
            withFile localCounterPath WriteMode (\h -> hPutStr h $ show s)
        shouldUpdateCounter r m =
            if (1 `elem` r) || ((maximum r) > m)
                then (True, maximum r) else (False, 0)
        extractTask fChan oChan = do
            empt <- atomically $ STC.isEmptyTChan oChan
            if empt
                then return 0
                else do
                    a <- atomically $ STC.readTChan oChan
                    if (ptStatus a == Just True)
                        then return (ptFrameSeqNr a)
                        -- This was a failed task, so resubmit it throuth the
                        -- failed Tasks channel to the 'assignPullTasks'
                        else BCH.writeChan fChan a >> return 0


--------------------------------------------------------------------------------
-- | Computes the first K parents, in descending order of their counter record.
-- Returns a list where the top K parents are identified by tuples of the form
-- (pIp, pPort, pLatestCounter).
getTopKParents ::
    [Parent]                        -- initial list of parents
    -> Int                          -- number of parents to return
    -> [(String, Int, Int)]         -- accumulator
    -> IO [(String, Int, Int)]
getTopKParents []     k accum = return $ take k accum
getTopKParents (p:xp) k accum = do
    ignore <- readMVar $ pIgnore p
    -- First, we check the Ignore flag.
    if ignore == True
        then getTopKParents xp k accum
        -- If the parent is not ignored, insert it in the accumulator list.
        else do
            tup <- parentToTuple p
            getTopKParents xp k $ R.insertBagBy compareByCounter tup accum
    where
        parentToTuple pa = do
            cnt <- readMVar $ pLatestCounter pa
            return (pIp pa, pPort pa, cnt)
        -- The accumulator list is ordered descendingly by the counter value.
        compareByCounter (_, _, cnt1) (_, _, cnt2) =
            compare cnt2 cnt1 -- descending order


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
        >>= (\mSeqNr ->
                if isJust mSeqNr
                    then do
                        deliverSeqNr (fromJust mSeqNr)
                        atomically $ STC.writeTChan oC (okT task)
                    else atomically $ STC.writeTChan oC (fdT task)
            ))
    where
        -- Transforms a Task into a failed Task, i.e., updates the ptStatus
        -- record field to False.
        fdT task = task { ptStatus = Just False }
        okT task = task { ptStatus = Just True }
        -- Pretty prints a K.TimeSpec data.
        showTime t = (show $ K.sec t) ++ "." ++ (show $ K.nsec t)
        -- Delivers (prints) a seq. number, tagged with the current real time.
        deliverSeqNr s = K.getTime (K.Realtime)
            >>= (\t -> putStrLn $ (showTime t) ++ " d " ++ (show s))


--------------------------------------------------------------------------------
-- | Pulls a frame from a given parent and returns the sequence number for that
-- frame. The frame is identified by a (sequence number, digest) tuple.
executePullTask ::
    PullTask
    -> IO (Maybe Int)
executePullTask task = do
    -- The port that serves frames is predefined: parentListeningPort = 80
    bytes <- pullBytes (ptParentIp task) parentListeningPort seqNr retryLimit
    if verifyDigest (ptFrameDigest task) bytes == True
        then persistFrame seqNr bytes >> return (Just seqNr)
        else
            putStrLn ((show seqNr) ++ ": Invalid frame! (len="
                        ++ (show $ L.length bytes) ++ ")")
            >> return Nothing
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
    -> Int  -- retry limit
    -> IO (L.ByteString)
pullBytes ip port seqNr retry = do
    result <- httpGetFrameBytes $ constructFrameURL ip port seqNr
    case result of
        Just bytes  -> return bytes
        Nothing     ->
            if retry > 0
                then do
                    putStrLn $ "We shall retry: (" ++ (show retry) ++ ") for "
                             ++ (show $ constructFrameURL ip port seqNr)
                    threadDelay (retryDelay*(retryLimit-retry+1) )
                    >> pullBytes ip port seqNr (retry-1)
                else return L.empty


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