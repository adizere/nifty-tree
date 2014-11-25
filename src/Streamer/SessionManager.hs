module Streamer.SessionManager where

import Streamer.Types
import Streamer.PullNodes


data SessionManager = SessionManager
    { sessionPullNodes  :: PullNodesList
    , frames            :: [Frame]
    }  deriving (Eq, Show)