module Streamer.Types where


data PullNode = PullNode
    { revision :: Int
    } deriving (Eq, Show)


data