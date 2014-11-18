module Streamer.Session where

import Streamer.Types
import Streamer.SessionManager


data Session = Session
    { id        :: Int
    , pullNodes :: PullNodesList
    , manager   :: SessionManager
    }  deriving (Eq, Show)


startSession :: Session -> Bool
startSession _ =
    True