import System.INotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)


doAddWatch :: INotify -> IO ()
doAddWatch ino = do
    let modif = Modify
    _ <- addWatch ino [modif] "/mnt/lpd-distlib/streamer/v1/digests.list" func
    return ()
    where
        func evt = do
            putStrLn $ show evt
            return ()

main = do
    ino <- initINotify
    doAddWatch ino
    forever $ do
        putStrLn "waiting for the worms to come.."
        threadDelay 1000000