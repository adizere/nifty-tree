module Streamer.SessionManager where


import Streamer.Parents
import Streamer.SessionManager.ParallelPull ( startParallelPull )

import System.IO ( Handle
                 , IOMode(ReadMode)
                 , openFile
                 , hGetLine
                 , hIsEOF
                 )
import Control.Concurrent               ( threadDelay, forkIO )
import Control.Concurrent.BoundedChan


-- Path to the file holding the list of digests
digestsFilePath :: FilePath
digestsFilePath = "/mnt/lpd-distlib/streamer/v1/digests.list"


-- How much to wait if the digests file EOF found, in microseconds
consumeDigestsFileDelay :: Int
consumeDigestsFileDelay = 1000000


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


-- | Consumes lines from a digest file and appends the lines to a BoundedChan.
-- Takes as arguments a file handle and a BoundedChan and a number. The third
-- parameter indicates how many lines to initially skip before starting to
-- append to the BoundedChan.
--
-- This function never returns (it will continue to call itself recursively),
-- therefore it should execute in its own thread.
--
-- Example:
-- >    handle  <- openFile "/tmp/a" ReadMode
-- >    chan    <- newBoundedChan 10
-- >    forkIO (consumeDgstFile handle chan)
--
-- Note: Apparently, BoundedChan is not exception safe (i.e. we can't do
-- killThread on the thread executing consumeDgstFile). For an exception safe
-- chan: Control.Concurrent.Chan (http://stackoverflow.com/a/9250200/919383)
consumeDgstFile :: Handle -> BoundedChan (String) -> Int -> IO ()
consumeDgstFile h c skipNr = do
    isEof <- hIsEOF h
    if isEof == True
        then threadDelay consumeDigestsFileDelay
        else do
            line <- hGetLine h
            if skipNr > 0
                then consumeDgstFile h c $ skipNr - 1
                else writeChan c line
    consumeDgstFile h c 0