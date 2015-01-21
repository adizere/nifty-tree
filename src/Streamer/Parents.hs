module Streamer.Parents where


import Streamer.HTTPClient
import Streamer.Neighbors
import Streamer.Util                (getShuffledList)

import Control.Concurrent.MVar
import Control.Concurrent           (threadDelay)
import Control.Monad                (forever)
import qualified Data.ByteString    as B


-- | How many microseconds to pause between subsequent parents checks.
checkParentsDelay :: Int
checkParentsDelay = 1000000     -- 1.000.000 us = 1 second


-- | Default port for HTTP requests to any parent
parentListeningPort :: Int
parentListeningPort = 80


data Parent = Parent
    { pIp              :: String
    , pPort            :: Int
    , pLatestCounter   :: MVar Int
    , pLatestETag      :: MVar B.ByteString
    , pIgnore          :: MVar Bool
    } deriving (Eq)


data ParentsSelection = ParentsSelection
    { psList        :: [Parent]
    , psRevision    :: Int
    } deriving (Eq, Show)


instance Show Parent where
    show Parent { pIp = ip, pPort = port } =
        "Parent (" ++ ip ++ ":" ++ (show port) ++ ")"


-- Parses the overlay info file and then selects from among the neighbors a set
-- of nodes that will serve as parents in the broadcast tree.
maybeSelectParents :: String -> IO (Maybe ParentsSelection)
maybeSelectParents sessionFileName = do
    mOverlayInfo <- maybeGetOverlayInfo sessionFileName
    case mOverlayInfo of
        Just overlayInfo -> do
            -- get a shuffled list of numbers to be user at node selection
            randomInts <- getShuffledList . length $ oiNeighbors overlayInfo
            -- now assemble the list of Parents and return it
            let (selNeighbors, rev) = assembleParents overlayInfo randomInts
            bN <- mapM transformNeighborToParent selNeighbors
            return . Just $ ParentsSelection {psList = bN, psRevision = rev}
        Nothing -> return Nothing
    where


assembleParents :: OverlayInfo -> [Int] -> ([Neighbor], Int)
assembleParents overlayInfo randomInts =
    if (oiSourceLink overlayInfo == True)
        -- If the source node is directly linked, then select it
        then ([sourceNode], (oiRevision overlayInfo))
        -- Otherwise, assemble some random nodes..
        else assembleRandomParents overlayInfo randomInts
    where
        sourceNode = Neighbor { nbIp = oiSourceIp overlayInfo
                              , nbPort = oiSourcePort overlayInfo
                              , nbCycleNumber = 0
                              , nbCycleDirection = 0 }


assembleRandomParents :: OverlayInfo -> [Int] -> ([Neighbor], Int)
assembleRandomParents overlayInfo randomInts =
    ( selectParents overlayInfo randomInts srcCyNum srcCyDir
    , oiRevision overlayInfo)
    where
        -- check if the source node is a neighbor & get its location
        (srcCyNum, srcCyDir)    = findNeighbor allNeighbors sIp sPort
        allNeighbors            = oiNeighbors overlayInfo  -- overlay neighbors
        sIp                     = oiSourceIp overlayInfo   -- source IP address
        sPort                   = oiSourcePort overlayInfo -- source Port number


selectParents :: OverlayInfo -> [Int] -> Int -> Int -> [Neighbor]
selectParents oi sList cNum cDir =
    depSet ++ fastSet
    where
        allN = oiNeighbors oi
        (depSet, uSList) = selectDependableSet allN sList cycleNum cycleDir
        fastSet = selectFastPathSet allN uSList cycleNum cycleDir
        cycleNum = if cNum == 0 then 1 else cNum
        cycleDir = if cDir == 0 then 1 else cDir


transformNeighborToParent :: Neighbor -> IO (Parent)
transformNeighborToParent Neighbor { nbIp = nIp, nbPort = nPort} = do
    lc <- newMVar (0)       :: IO (MVar Int)
    le <- newMVar (B.empty) :: IO (MVar B.ByteString)
    ig <- newMVar (False)   :: IO (MVar Bool)
    return Parent { pIp               = nIp
                  , pPort             = nPort
                  , pLatestCounter    = lc
                  , pLatestETag       = le
                  , pIgnore           = ig }


selectDependableSet :: [Neighbor] -> [Int] -> Int -> Int -> ([Neighbor], [Int])
selectDependableSet aN sList cNum cDir =
    (selectedNodes, uSList)
    where
        (nCount, allFromCluster) = getAllNeighborsFromCluster aN cNum cDir
        majCount = nCount `div` 2 + 1
        (positions, uSList) = splitAt majCount sList
        nodePositions = map (\p -> p `mod` nCount) positions
        selectedNodes = map (\pos -> allFromCluster!!pos) nodePositions


selectFastPathSet :: [Neighbor] -> [Int] -> Int -> Int -> [Neighbor]
selectFastPathSet aN sList ignoreNum ignoreDir =
    selectedNodes
    where
        clusters = filter
                    ((ignoreNum, ignoreDir) /=)
                    $ getClusterIdentities aN
        selectedNodes = zipWith func sList clusters
        func position (cNum, cDir) = allFromClustr!!pos
            where
                (clCnt, allFromClustr) = getAllNeighborsFromCluster aN cNum cDir
                pos = position `mod` clCnt


getClusterIdentities :: [Neighbor] -> [(Int, Int)]
getClusterIdentities allNeighbors =
    foldl func [] allNeighbors
    where
        func soFar n = if (nbCycleNumber n, nbCycleDirection n) `elem` soFar
                            then soFar
                            else (nbCycleNumber n, nbCycleDirection n):soFar


getAllNeighborsFromCluster :: [Neighbor] -> Int -> Int -> (Int, [Neighbor])
getAllNeighborsFromCluster allNeighbors cycleNum cycleDir =
    (foundCount, foundNodes)
    where
        foundNodes = [ n | n <- allNeighbors
                        , nbCycleNumber n == cycleNum
                        , nbCycleDirection n == cycleDir ]
        foundCount = length foundNodes


-- | Searches for a node among the neighbors list. If the node is found, the
-- cycle number and direction for that node is returned. Otherwise, the
-- function returns (0, 0).
--
-- The node is identified by a String (Ip) and an Int (Port).
findNeighbor :: [Neighbor] -> String -> Int -> (Int, Int)
findNeighbor [] _ _ = (0, 0)
findNeighbor (n:ns) sIp sPort
    | nbIp n == sIp && nbPort n == sPort = (cyNum, cyDir)
    | otherwise = findNeighbor ns sIp sPort
    where
        cyNum = nbCycleNumber n
        cyDir = nbCycleDirection n

-- | Verifies the counter at each parent and updates the fields 'pLatestCounter'
-- and 'pLatestETag'.
-- This function never returns, so it will check every parent inifinitely often.
checkParents :: ParentsSelection -> IO ()
checkParents pSelection =
    forever $ do
        mapM_ checkOneParent $ psList pSelection
        threadDelay checkParentsDelay
        return ()


-- | Performs the check of one given parent, updating the coresponing record
-- fields of counter and ETag.
checkOneParent :: Parent -> IO ()
checkOneParent cp = do
    putStrLn $ "[pchecker] Checking parent " ++ show cp
    cEtag <- readMVar (pLatestETag cp)
    mCntr <- httpGetCounter
                (constructCounterURL (pIp cp) parentListeningPort) cEtag
    case mCntr of
        Just (counter, etag) ->
            (swapMVar (pLatestCounter cp) counter)
            >>= (\v -> putStrLn $ "[pchecker] Previous val was: " ++ (show v))
            >> (swapMVar (pLatestETag cp) etag) >> return ()
        Nothing -> return ()