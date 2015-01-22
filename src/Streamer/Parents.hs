module Streamer.Parents where


import Streamer.HTTPClient
import Streamer.Neighbors
import Streamer.Util                ( getShuffledList
                                    , uniqueWrappedNumbers )

import Control.Concurrent.MVar
import Control.Concurrent           ( threadDelay )
import Control.Monad                ( forever )
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


-- | Parses the overlay info file (session.json) and then selects from among the
-- neighbors a set of nodes that will serve as parents in the broadcast tree.
maybeSelectParents ::
    String                         -- ^ Path to overlay info file (session.json)
    -> IO (Maybe ParentsSelection)
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


-- | Assembles a list Neighbors that will serve as parents in the broadcast
-- tree. It obtains the Neighbors from a given OverlayInfo structure. Along with
-- the selected parents, it returns a revision number, to track changes in time.
--
-- If the OverlayInfo structure reports that the current node is directly linked
-- with the source node, then it selects the source node as the unique parent.
--
-- If the source node is not directly link, then this function selects the
-- neighbors pseudo-randomly; in order to keep this computation pure, it expects
-- a list of random numbers generated at the caller.
assembleParents ::
    OverlayInfo         -- ^ An OverlayInfo with some Neighbors
    -> [Int]            -- ^ A list of random numbers
    -> ([Neighbor], Int)
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


-- | Assembles a list of randomly selected nodes from an OverlayInfo structure.
-- This function given precedence to nodes co-located in the same cluster as
-- the source node.
assembleRandomParents ::
    OverlayInfo         -- ^ An OverlayInfo with some Neighbors
    -> [Int]            -- ^ A list of random numbers
    -> ([Neighbor], Int)
assembleRandomParents overlayInfo randomInts =
    ( selectParents overlayInfo randomInts srcCyNum srcCyDir
    , oiRevision overlayInfo)
    where
        -- check if the source node is a neighbor & get its location
        (srcCyNum, srcCyDir)    = findNeighbor allNeighbors sIp sPort
        allNeighbors            = oiNeighbors overlayInfo  -- overlay neighbors
        sIp                     = oiSourceIp overlayInfo   -- source IP address
        sPort                   = oiSourcePort overlayInfo -- source Port number


-- | Selects random nodes using 'selectDependableSet' and 'selectFastPathSet'
selectParents ::
    OverlayInfo   -- ^ An OverlayInfo with some Neighbors
    -> [Int]      -- ^ A list of random numbers
    -> Int        -- ^ Source node location, relative to current node
                  -- (cycle number)
    -> Int        -- ^ Source node location, relative to current node
                  -- (cycle direction)
    -> [Neighbor] -- ^ The list of selected nodes
selectParents oi sList cNum cDir =
    depSet ++ fastSet
    where
        allN = oiNeighbors oi
        (depSet, uSList) = selectDependableSet allN sList cycleNum cycleDir
        fastSet = selectFastPathSet allN uSList cycleNum cycleDir
        cycleNum = if cNum == -1 then 0 else cNum
        cycleDir = if cDir == -1 then 0 else cDir


-- | Transforms a Neighbor structure into a new Parent structure; the resulting
-- Parent retains the Ip and the Port of the original Neighbor.
transformNeighborToParent ::
    Neighbor        -- ^ A Neighbor
    -> IO (Parent)  -- ^ The resulting Parent
transformNeighborToParent Neighbor { nbIp = nIp, nbPort = nPort} = do
    lc <- newMVar (0)       :: IO (MVar Int)
    le <- newMVar (B.empty) :: IO (MVar B.ByteString)
    ig <- newMVar (False)   :: IO (MVar Bool)
    return Parent { pIp               = nIp
                  , pPort             = nPort
                  , pLatestCounter    = lc
                  , pLatestETag       = le
                  , pIgnore           = ig }


-- | Selects the dependable set of Neighbors that will serve as parents for the
-- current node.
--
-- The dependable set is represented by a majority of nodes of a given cluster.
selectDependableSet ::
    [Neighbor]              -- ^ All the Neighbors of the current node
    -> [Int]                -- ^ A list of random numbers
    -> Int                  -- ^ Cycle number for the selected cluster
    -> Int                  -- ^ Cycle direction for the selected cluster
    -> ([Neighbor], [Int])  -- ^ Selected set of neighbors; unused randoms
selectDependableSet aN sList cNum cDir =
    (selectedNodes, uSList)
    where
        (nCount, allFromCluster) = getAllNeighborsFromCluster aN cNum cDir
        majCount = nCount `div` 2 + 1
        -- Split the list of randoms, we only need the first majCount numbers
        (absoluteRands, uSList) = splitAt majCount sList
        -- Make the selected random numbers unique and wrap them around nCount
        nrwRands = uniqueWrappedNumbers absoluteRands nCount []
        selectedNodes = map (\pos -> allFromCluster!!pos) nrwRands


-- | Selects a Neighbor from each cluster, except for a selected cluster. The
-- selected cluster is passed as an argument (cycle number, cycle direction).
selectFastPathSet ::
    [Neighbor]      -- ^ All the Neighbors of the current node
    -> [Int]        -- ^ A list of random numbers
    -> Int          -- ^ Cycle number for the selected cluster
    -> Int          -- ^ Cycle direction for the selected cluster
    -> [Neighbor]   -- ^ Selected set of neighbors, one per cluster.
selectFastPathSet aN sList ignoreNum ignoreDir =
    selectedNodes
    where
        clusters = filter ((ignoreNum, ignoreDir) /=) $ getClusterIdentities aN
        selectedNodes = zipWith func sList clusters
        func position (cNum, cDir) = allFromClustr!!pos
            where
                (clCnt, allFromClustr) = getAllNeighborsFromCluster aN cNum cDir
                pos = position `mod` clCnt


-- | Parses a list of Neighbors and returs the list of unique clusters of these
-- Neighbors.
getClusterIdentities ::
    [Neighbor]      -- ^ A list of Neighbors
    -> [(Int, Int)] -- ^ Unique list of clusters for these Neighbors
getClusterIdentities allNeighbors =
    foldl func [] allNeighbors
    where
        func soFar n = if (nbCycleNumber n, nbCycleDirection n) `elem` soFar
                            then soFar
                            else (nbCycleNumber n, nbCycleDirection n):soFar


-- | Returns all the Neighbors located in a selected cluster.
getAllNeighborsFromCluster ::
    [Neighbor]              -- ^ A list of Neighbors
    -> Int                  -- ^ Selected cluster location: cycle number
    -> Int                  -- ^ Selected cluster location: cycle direction
    -> (Int, [Neighbor])    -- ^ Neighbors in the given cluster
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
findNeighbor [] _ _ = (-1, -1)
findNeighbor (n:ns) sIp sPort
    | nbIp n == sIp && nbPort n == sPort = (cyNum, cyDir)
    | otherwise = findNeighbor ns sIp sPort
    where
        cyNum = nbCycleNumber n
        cyDir = nbCycleDirection n

-- | Verifies the counter at each parent and updates the fields 'pLatestCounter'
-- and 'pLatestETag'.
-- This function never returns, so it will check every parent at a regular
-- interval for as long as the program executes.
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
    -- putStrLn $ "[pchecker] Checking parent " ++ show cp
    cEtag <- readMVar (pLatestETag cp)
    mCntr <- httpGetCounter
                (constructCounterURL (pIp cp) parentListeningPort) cEtag
    case mCntr of
        Just (counter, etag) ->
            (swapMVar (pLatestCounter cp) counter)
            -- >>= (\v -> putStrLn $ "[pchecker] Previous val was: " ++ (show v))
            >> (swapMVar (pLatestETag cp) etag) >> return ()
        Nothing -> return ()