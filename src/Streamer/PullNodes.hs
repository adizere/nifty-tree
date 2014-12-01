{-# LANGUAGE OverloadedStrings #-}

module Streamer.PullNodes where


import Streamer.Neighbors
import Streamer.Util (getShuffledList)


data PullNode = PullNode
    { pnIp      :: String
    , pnPort    :: Int
    } deriving (Eq, Show)


data PullNodesList = PullNodesList
    { pnlActiveNode     :: PullNode
    , pnlBackupNodes    :: [PullNode]
    , pnlRevision       :: Int
    } deriving (Eq, Show)


maybeGetPullNodes :: String -> IO (Maybe PullNodesList)
maybeGetPullNodes sessionFileName = do
    mOverlayInfo <- maybeGetOverlayInfo sessionFileName
    case mOverlayInfo of
        Just overlayInfo -> do
            -- get a shuffled list of numbers to be user at node selection
            sList <- getShuffledList . length $ oiNeighbors overlayInfo
            -- now assemble the list of PullNodes and return it
            return . Just $ assemblePullNodes overlayInfo sList
        Nothing -> return Nothing


assemblePullNodes :: OverlayInfo -> [Int] -> PullNodesList
assemblePullNodes overlayInfo sList =
    selectPullNodes overlayInfo sList srcCyNum srcCyDir
    where
        -- check if the source node is a neighbor & get its location
        (srcCyNum, srcCyDir)    = findNode allNeighbors sIp sPort
        allNeighbors            = oiNeighbors overlayInfo  -- overlay neighbors
        sIp                     = oiSourceIp overlayInfo   -- source IP address
        sPort                   = oiSourcePort overlayInfo -- source Port number


selectPullNodes :: OverlayInfo -> [Int] -> Int -> Int -> PullNodesList
selectPullNodes oi sList cNum cDir =
    PullNodesList {pnlActiveNode = aN, pnlBackupNodes = bN, pnlRevision = rev}
    where
        allN = oiNeighbors oi
        (depSet, uSList) = selectDependableSet allN sList cycleNum cycleDir
        fastSet = selectFastPathSet allN uSList cycleNum cycleDir
        aN = head depSet
        bN = (drop 1 depSet) ++ fastSet
        rev = oiRevision oi
        cycleNum = if cNum == 0 then 1 else cNum
        cycleDir = if cDir == 0 then 1 else cDir


transformNeighborToPullNode :: Neighbor -> PullNode
transformNeighborToPullNode Neighbor { nbIp = nIp, nbPort = nPort} =
    PullNode { pnIp = nIp, pnPort = nPort }


selectDependableSet :: [Neighbor] -> [Int] -> Int -> Int -> ([PullNode], [Int])
selectDependableSet aN sList cNum cDir =
    (ds, uSList)
    where
        (nCount, allFromCluster) = getAllNodesFromCluster aN cNum cDir
        majCount = nCount `div` 2 + 1
        (positions, uSList) = splitAt majCount sList
        nodePositions = map (\p -> p `mod` nCount) positions
        selectedNodes = map (\pos -> allFromCluster!!pos) nodePositions
        ds = map transformNeighborToPullNode selectedNodes


selectFastPathSet :: [Neighbor] -> [Int] -> Int -> Int -> [PullNode]
selectFastPathSet aN sList ignoreNum ignoreDir =
    map transformNeighborToPullNode selectedNodes
    where
        clusters = filter
                    ((ignoreNum, ignoreDir) /=)
                    $ getClusterIdentities aN
        selectedNodes = zipWith func sList clusters
        func position (cNum, cDir) = allFromCluster!!pos
            where
                (clCount, allFromCluster) = getAllNodesFromCluster aN cNum cDir
                pos = position `mod` clCount


getClusterIdentities :: [Neighbor] -> [(Int, Int)]
getClusterIdentities allNeighbors =
    foldl func [] allNeighbors
    where
        func soFar n = if (nbCycleNumber n, nbCycleDirection n) `elem` soFar
                            then soFar
                            else (nbCycleNumber n, nbCycleDirection n):soFar


getAllNodesFromCluster :: [Neighbor] -> Int -> Int -> (Int, [Neighbor])
getAllNodesFromCluster allNeighbors cycleNum cycleDir =
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
findNode :: [Neighbor] -> String -> Int -> (Int, Int)
findNode [] _ _ = (0, 0)
findNode (n:ns) sIp sPort
        | nbIp n == sIp && nbPort n == sPort = (cyNum, cyDir)
        | otherwise = findNode ns sIp sPort
        where
            cyNum = nbCycleNumber n
            cyDir = nbCycleDirection n