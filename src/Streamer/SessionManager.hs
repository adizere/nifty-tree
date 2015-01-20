module Streamer.SessionManager where


import Streamer.Parents
import Streamer.SessionManager.ParallelPull ( startParallelPull )
import Streamer.SessionManager.DigestsFile  ( consumeDgstFile )

import Control.Concurrent.BoundedChan
import Control.Concurrent               ( forkIO )
import System.IO                        ( IOMode(ReadMode), openFile )


-- Path to the file holding the list of digests
digestsFilePath :: FilePath
digestsFilePath = "/opt/streamer/digests.list"


-- Length of the BoundedChan which holds the digests
digestsChanLength :: Int
digestsChanLength = 5


data SessionManager = SessionManager
    { smParents   :: ParentsSelection
    , smFramesSeqNr :: [Int]
    }  deriving (Eq)


-- SessionManager is an instance of Show typeclass, and we want a simplified
-- string representation.
instance Show SessionManager where
    show sm = "{smFramesSeqNr = " ++ (show $ smFramesSeqNr sm) ++ "}"


-- | Main function executed by each SessionManager.
startSessionManager :: SessionManager -> IO ()
startSessionManager sManager = do
    h    <- openFile digestsFilePath ReadMode
    chan <- newBoundedChan digestsChanLength
    _    <- forkIO (consumeDgstFile h chan 0)
    _    <- forkIO (checkParents $ smParents sManager)
    startParallelPull (psList $ smParents sManager)
                      chan
                      (smFramesSeqNr sManager)
    return ()