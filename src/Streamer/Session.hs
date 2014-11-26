module Streamer.Session
( sessionDirectory
, startSession
, getAllSessions
, getSessionIdsFromSessionHandles
, finishedMVar
, SessionHandle
) where


import Streamer.PullNodes
import Streamer.SessionManager
import Data.Char
import System.Directory
import Control.Concurrent
import Control.Monad (forever)
import Control.Exception.Base (finally)


---------------------------------------
--- public functions and data types
---

-- directory with session file descriptors
sessionDirectory :: FilePath
sessionDirectory = "."


-- each SessionHandle describes a running session
data SessionHandle = SessionHandle
    { runningSessionId  :: String -- the same significance as Session.sessionId
    , threadId          :: ThreadId
    , finishedMVar      :: MVar (Int)
    } deriving (Eq)


instance Show SessionHandle where
    show SessionHandle { runningSessionId = r
                       , threadId = t
                       , finishedMVar = _
                       } = "SessionHandle (id=" ++ (show r)
                        ++ "," ++ (show t) ++ ")"

startSession :: String -> IO SessionHandle
startSession sId = do
    m <- newEmptyMVar
    -- spark a new thread, which will handle exclusively the session with id sId
    tId <- forkIO (finally (sessionMainLoop sId) (putMVar m 1))
    return SessionHandle { runningSessionId = sId
                         , threadId         = tId
                         , finishedMVar     = m }


getAllSessions :: IO [String]
getAllSessions = do
    getSessionsFromDirectory sessionDirectory


getSessionIdsFromSessionHandles :: [SessionHandle] -> [String]
getSessionIdsFromSessionHandles iSessions =
    -- use the accessor function to extract the sessionId from each iSession
    [ runningSessionId session | session <- iSessions ]


---------------------------------------
--- private functions and data types
---


data Session = Session
    { sessionId :: String
    , pullNodes :: PullNodesList
    -- , manager   :: SessionManager
    } deriving (Eq, Show)


getSessionsFromDirectory :: FilePath -> IO [String]
getSessionsFromDirectory path = do
        allFiles <- getDirectoryContents path
        let ids = map getSessionIdFromFileName allFiles
        return [ sId | sId <- ids, length sId > 0]


-- extracts the id of the session from the fileName
-- fileName has the form: "sessionX.json"
-- where X is a string of digits, representing the id
--
-- Usage:
-- getSessionIdFromFileName "session456.json" = "456"
-- getSessionIdFromFileName "session.json" = ""
getSessionIdFromFileName :: String -> String
getSessionIdFromFileName fileName
    | length fileName <= 12         = ""
    | take 7 fileName /= "session"  = ""
    | otherwise                     =
        [ x | x <- snd . splitAt 7 $ fileName, isDigit x]


-- expects the id of a session
-- returns the filename where that
assembleSessionFilePath :: String -> String
assembleSessionFilePath sId =
    sessionDirectory ++ "/" ++ "session" ++ sId ++ ".json"


-- the main function executed by every Session
sessionMainLoop :: String -> IO ()
sessionMainLoop sId = forever $ do
    threadDelay 1000000
    session <- assembleSession sId
    putStrLn $ sId ++ " FINISHED!!" ++ (show session)


assembleSession :: String -> IO Session
assembleSession sId = do
    let sessionFileName = assembleSessionFilePath sId
    putStrLn $ "Reading session from file " ++ sessionFileName
    selectedNodes <- selectPullNodesFromFile sessionFileName
    return Session { sessionId = sId, pullNodes = selectedNodes}