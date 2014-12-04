import System.IO
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent (forkIO)


digestsFilePath :: String
digestsFilePath = "/mnt/lpd-distlib/streamer/v1/digests.list"


checkFile :: Handle -> BoundedChan (String) -> Int -> IO ()
checkFile handle chan skip = do
    isEof <- hIsEOF handle
    if isEof == True
        then do
            threadDelay 1000000
            checkFile handle chan 0
        else do
            hasLine <- hWaitForInput handle 67
            if hasLine == True
                then do
                    line <- hGetLine handle
                    if skip > 0
                        then checkFile handle chan $ skip - 1
                        else do
                            writeChan chan line
                            checkFile handle chan 0
                else
                    checkFile handle chan 0


main = do
    handle  <- openFile digestsFilePath ReadMode
    chan    <- newBoundedChan 10
    forkIO (checkFile handle chan 1)
    forever $ do
        putStrLn "attempting to read a line.."
        a <- readChan chan
        putStrLn a