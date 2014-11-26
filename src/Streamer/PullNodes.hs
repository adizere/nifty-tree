{-# LANGUAGE OverloadedStrings #-}

module Streamer.PullNodes where


import Streamer.Neighbors

import Control.Monad
import Control.Applicative
import Data.Aeson


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
        Just overlayInfo    -> return . Just $ assemblePullNodes overlayInfo
        Nothing             -> return Nothing


assemblePullNodes :: OverlayInfo -> PullNodesList
assemblePullNodes overlayInfo
    | srcIsNeighbor == True =
            assemblePullNodesWithSource overlayInfo srcCyNum srcCyDir
    | otherwise =
            assemblePullNodesWithSource overlayInfo srcCyNum srcCyDir
    where
        -- check if the source node is a neighbor & get its location
        (srcIsNeighbor, srcCyNum, srcCyDir) =
            sourceNodeIsNeighbor
                (oiNeighbors overlayInfo)   -- overlay neighbors list
                (oiSourceIp overlayInfo)    -- source IP address
                (oiSourcePort overlayInfo)  -- source Port number


assemblePullNodesWithSource :: OverlayInfo -> Int -> Int -> PullNodesList
assemblePullNodesWithSource oi cNum cDir =
    PullNodesList {pnlActiveNode = pNode, pnlBackupNodes = sNodes, pnlRevision = 1}
    where
        pNode = PullNode {pnIp = "aa", pnPort = 45}
        sNodes = getMajorityFromCluster (oiNeighbors oi) cNum cDir


transformNeighborToPullNode :: Neighbor -> PullNode
transformNeighborToPullNode Neighbor { nbIp = nIp, nbPort = nPort} =
    PullNode { pnIp = nIp, pnPort = nPort }


getMajorityFromCluster :: [Neighbor] -> Int -> Int -> [PullNode]
getMajorityFromCluster allNeighbors cycleNum cycleDir =
    map transformNeighborToPullNode $ take majCount allFromCluster
    where
        (nCount, allFromCluster) = getAllNodesFromCluster allNeighbors cycleNum cycleDir
        majCount = nCount `div` 2 + 1


getAllNodesFromCluster :: [Neighbor] -> Int -> Int -> (Int, [Neighbor])
getAllNodesFromCluster allNeighbors cycleNum cycleDir =
    (foundCount, foundNodes)
    where
        foundNodes = [ n | n <- allNeighbors
                        , nbCycleNumber n == cycleNum
                        , nbCycleDirection n == cycleDir ]
        foundCount = length foundNodes


-- | Returns True and cycle number and cycle direction
-- if the source node is among the Neighbor list.
-- Cycle number and cycle direction represents the location of the source node
-- The source node is identified by a String (Ip) and an Int (Port)
sourceNodeIsNeighbor :: [Neighbor] -> String -> Int -> (Bool, Int, Int)
sourceNodeIsNeighbor [] _ _ = (False, 0, 0)
sourceNodeIsNeighbor (n:ns) sIp sPort
        | nbIp n == sIp && nbPort n == sPort = (True, cyNum, cyDir)
        | otherwise = sourceNodeIsNeighbor ns sIp sPort
        where
            cyNum = nbCycleNumber n
            cyDir = nbCycleDirection n