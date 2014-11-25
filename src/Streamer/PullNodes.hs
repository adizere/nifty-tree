{-# LANGUAGE OverloadedStrings #-}

module Streamer.PullNodes where


import Streamer.Neighbors

import Control.Monad
import Control.Applicative
import Data.Aeson


data PullNode = PullNode
    { ip        :: String
    , port      :: Int
    } deriving (Eq, Show)


data PullNodesList = PullNodesList
    { activeNode    :: PullNode
    , backupNodes   :: [PullNode]
    , revision      :: Int
    } deriving (Eq, Show)


instance FromJSON PullNode where
    parseJSON (Object v) = PullNode <$>
                           v .: "ip" <*>
                           v .: "port"

    parseJSON _          = mzero


selectPullNodesFromFile :: String -> IO PullNodesList
selectPullNodesFromFile sessionFileName = do
    let pNode = PullNode {ip = "aa", port = 45}
    allNeighbors <- getAllNeighboringNodes sessionFileName
    return PullNodesList {activeNode = pNode, backupNodes = [pNode], revision = 1}