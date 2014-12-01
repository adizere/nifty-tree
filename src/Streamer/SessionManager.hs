module Streamer.SessionManager where

import Streamer.Types
import Streamer.PullNodes


data SessionManager = SessionManager
    { smPullNodes   :: PullNodesList
    , smFrames      :: [Frame]
    }  deriving (Eq)


instance Show SessionManager where
    show sm = "{smFrames = " ++ (show $ smFrames sm) ++ " }"