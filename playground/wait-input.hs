import System.IO
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent (forkIO)


digestsFilePath :: String
digestsFilePath = "/mnt/lpd-distlib/streamer/v1/digests.list"


checkFile :: Handle -> BoundedChan (String) -> IO ()
checkFile handle chan = do
    isEof <- hIsEOF handle
    if isEof == True
        then do
            threadDelay 1000000
            checkFile handle chan
        else do
            hasLine <- hWaitForInput handle 67
            if hasLine == True
                then do
                    line <- hGetLine handle
                    writeChan chan line
                    checkFile handle chan
                else
                    checkFile handle chan


main = do
    handle  <- openFile digestsFilePath ReadMode
    chan    <- newBoundedChan 10
    forkIO (checkFile handle chan)
    forever $ do
        putStrLn "attempting to read a line.."
        a <- readChan chan
        putStrLn a