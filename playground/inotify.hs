import System.INotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.IO


digestsFilePath :: String
digestsFilePath = "/mnt/lpd-distlib/streamer/v1/digests.list"


doAddWatch :: INotify -> IO ()
doAddWatch ino = do
    let modif = CloseWrite
    -- Note: the callback to the provided function is sequential (synchronizedss)
    handle <- openFile digestsFilePath ReadMode
    _ <- addWatch ino [modif] digestsFilePath (func handle)
    return ()
    where
        func handle evt = do
            line <- hGetLine handle
            -- putStrLn $ show evt
            putStrLn line
            -- threadDelay 100000
            -- putStrLn "finished waiting.."
            return ()


main = do
    ino <- initINotify
    doAddWatch ino
    forever $ do
        -- putStrLn "waiting for the worms to come.."
        threadDelay 1000000