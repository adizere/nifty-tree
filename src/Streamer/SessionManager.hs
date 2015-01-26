module Streamer.SessionManager where


import Streamer.Parents
import Streamer.SessionManager.ParallelPull ( startParallelPull )
import Streamer.SessionManager.DigestsFile  ( consumeDgstFile )

import Control.Concurrent                           ( forkIO )
import qualified Control.Concurrent.STM.TQueue      as STQ


-- Path to the file holding the list of digests
digestsFilePath :: FilePath
digestsFilePath = "/opt/streamer/digests.list"


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
    putStrLn $ "Selected parents: " ++ (show $ smParents sManager)
    queue <- STQ.newTQueueIO :: IO (STQ.TQueue String)
    _     <- forkIO (consumeDgstFile digestsFilePath 0 queue)
    _     <- forkIO (checkParents $ smParents sManager)
    startParallelPull (psList $ smParents sManager)
                      queue
                      (smFramesSeqNr sManager)
    return ()