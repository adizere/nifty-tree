{-# LANGUAGE OverloadedStrings #-}

module Streamer.Parents where


import Streamer.Neighbors
import Streamer.Util (getShuffledList)


data Parent = Parent
    { pnIp      :: String
    , pnPort    :: Int
    } deriving (Eq, Show)


data ParentsSelection = ParentsSelection
    { pnlBackupNodes    :: [Parent]
    , pnlRevision       :: Int
    } deriving (Eq, Show)


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
            return . Just $ assembleParents overlayInfo randomInts
        Nothing -> return Nothing


assembleParents :: OverlayInfo -> [Int] -> ParentsSelection
assembleParents overlayInfo randomInts =
    selectParents overlayInfo randomInts srcCyNum srcCyDir
    where
        -- check if the source node is a neighbor & get its location
        (srcCyNum, srcCyDir)    = findNeighbor allNeighbors sIp sPort
        allNeighbors            = oiNeighbors overlayInfo  -- overlay neighbors
        sIp                     = oiSourceIp overlayInfo   -- source IP address
        sPort                   = oiSourcePort overlayInfo -- source Port number


selectParents :: OverlayInfo -> [Int] -> Int -> Int -> ParentsSelection
selectParents oi sList cNum cDir =
    ParentsSelection {pnlBackupNodes = bN, pnlRevision = rev}
    where
        allN = oiNeighbors oi
        (depSet, uSList) = selectDependableSet allN sList cycleNum cycleDir
        fastSet = selectFastPathSet allN uSList cycleNum cycleDir
        bN = depSet ++ fastSet
        rev = oiRevision oi
        cycleNum = if cNum == 0 then 1 else cNum
        cycleDir = if cDir == 0 then 1 else cDir


transformNeighborToParent :: Neighbor -> Parent
transformNeighborToParent Neighbor { nbIp = nIp, nbPort = nPort} =
    Parent { pnIp = nIp, pnPort = nPort }


selectDependableSet :: [Neighbor] -> [Int] -> Int -> Int -> ([Parent], [Int])
selectDependableSet aN sList cNum cDir =
    (ds, uSList)
    where
        (nCount, allFromCluster) = getAllNeighborsFromCluster aN cNum cDir
        majCount = nCount `div` 2 + 1
        (positions, uSList) = splitAt majCount sList
        nodePositions = map (\p -> p `mod` nCount) positions
        selectedNodes = map (\pos -> allFromCluster!!pos) nodePositions
        ds = map transformNeighborToParent selectedNodes


selectFastPathSet :: [Neighbor] -> [Int] -> Int -> Int -> [Parent]
selectFastPathSet aN sList ignoreNum ignoreDir =
    map transformNeighborToParent selectedNodes
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