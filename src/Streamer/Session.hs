module Streamer.Session
( getAllSessions
, startSession
, extractSessionIdsFromSession
, Session
) where


import Streamer.Types
import Streamer.SessionManager
import Data.Char
import System.Directory
import Control.Concurrent


-- directory with session file descriptors
sessionDirectory :: FilePath
sessionDirectory = "."


data Session = Session
    { sessionId :: String
    , threadId  :: ThreadId
    -- , pullNodes :: PullNodesList
    -- , manager   :: SessionManager
    } deriving (Eq, Show)


startSession :: String -> IO Session
startSession sId = do
    tId <- forkIO (do threadDelay 100000
                      putStrLn "FINISHED!!")
    return Session { sessionId  = sId
                   , threadId   = tId }


getAllSessions :: IO [String]
getAllSessions = do
    getSessionsFromDirectory sessionDirectory


getSessionsFromDirectory :: FilePath -> IO [String]
getSessionsFromDirectory path = do
        allFiles <- getDirectoryContents path
        let ids = map getSessionIdFromFileName allFiles
        return [ sId | sId <- ids, length sId > 0]


getSessionIdFromFileName :: String -> String
getSessionIdFromFileName fileName
    | length fileName <= 12         = ""
    | take 7 fileName /= "session"  = ""
    | otherwise                     =
        [ x | x <- snd . splitAt 7 $ fileName, isDigit x]


extractSessionIdsFromSession :: [Session] -> [String]
extractSessionIdsFromSession iSessions =
    -- use the accessor function to extract the sessionId from each iSession
    [ sessionId session | session <- iSessions ]