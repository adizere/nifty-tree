module Streamer.Session
( joinSession
, getAllSessions
, SessionActor
, startSessionActor
) where


import Streamer.Types
import Streamer.SessionManager
import Streamer.Util
import Data.Char
import System.Directory
import Control.Concurrent


-- directory with sessions
sessionDirectory :: FilePath
sessionDirectory = "."


data Session = Session
    { id        :: String
    , pullNodes :: PullNodesList
    , manager   :: SessionManager
    } deriving (Eq, Show)


joinSession :: String -> IO ()
joinSession a = do
    putStrLn $ "Joining session " ++ a
    -- let s = Session


startSession :: Session -> Bool
startSession _ =
    True


getAllSessions :: IO [String]
getAllSessions = do
    getSessionsFromDirectory sessionDirectory


getSessionsFromDirectory :: FilePath -> IO [String]
getSessionsFromDirectory path = do
        allFiles <- getDirectoryContents sessionDirectory
        let ids = map getSessionIdFromFileName allFiles
        return [ sessionId | sessionId <- ids, length sessionId > 0]


getSessionIdFromFileName :: String -> String
getSessionIdFromFileName fileName
    | length fileName <= 12         = ""
    | take 7 fileName /= "session"  = ""
    | otherwise                     =
        [ x | x <- snd . splitAt 7 $ fileName, isDigit x]



data SessionActor = SessionActor
    { sessionActorId :: String
    , threadId :: ThreadId
    } deriving (Show)

-- instance Show SessionActor where
--     show (SessionActor {sessionActorId=sId, threadId=tId}) =
--             show sId ++ "/" ++ show tId

startSessionActor :: String -> IO SessionActor
startSessionActor actorId = do
    tId <- forkIO (do threadDelay 100000
                      putStrLn "FINISHED!!!")
    return SessionActor {sessionActorId=actorId,threadId=tId}