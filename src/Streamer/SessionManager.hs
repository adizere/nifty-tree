module Streamer.SessionManager where

import Streamer.Types

data SessionManager = SessionManager
    { pullNodes :: PullNodesList
    , frames    :: [Frame]
    }  deriving (Eq, Show)