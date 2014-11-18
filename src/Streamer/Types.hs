module Streamer.Types where


import qualified Data.ByteString.Lazy   as L


data PullNode = PullNode
    { revision  :: Int
    , ip        :: String
    , port      :: Int
    } deriving (Eq, Show)


data PullNodesList = PullNodesList
    { activeNode    :: PullNode
    , backupNodes   :: [PullNode]
    } deriving (Eq, Show)


data Frame = Frame
    { seqNr     :: Int
    , digest    :: String
    , content   :: L.ByteString
    } deriving (Eq, Show)