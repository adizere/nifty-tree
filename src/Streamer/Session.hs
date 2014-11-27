module Streamer.Session
( sessionDirectory
, startSession
, getAllSessions
, getSessionIdsFromSessionHandles
, shMVar
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
    { shSessionId   :: String -- the same significance as Session.ssId
    , shThreadId    :: ThreadId
    , shMVar        :: MVar (Int)
    } deriving (Eq)


instance Show SessionHandle where
    show SessionHandle { shSessionId = r , shThreadId = t } =
        "SessionHandle (id=" ++ (show r) ++ "," ++ (show t) ++ ")"

startSession :: String -> IO SessionHandle
startSession sId = do
    m <- newEmptyMVar
    -- spark a new thread, which will handle exclusively the session with id sId
    tId <- forkIO (finally (sessionMainLoop sId) (putMVar m 1))
    return SessionHandle { shSessionId  = sId
                         , shThreadId   = tId
                         , shMVar       = m }


getAllSessions :: IO [String]
getAllSessions = do
    getSessionsFromDirectory sessionDirectory


getSessionIdsFromSessionHandles :: [SessionHandle] -> [String]
getSessionIdsFromSessionHandles iSessions =
    -- use the accessor function to extract the sessionId from each iSession
    [ shSessionId session | session <- iSessions ]


---------------------------------------
--- private functions and data types
---


data Session = Session
    { ssId          :: String
    , ssPullNodes   :: PullNodesList
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
-- returns the filename where that session is described (.json file)
assembleSessionFilePath :: String -> String
assembleSessionFilePath sId =
    sessionDirectory ++ "/" ++ "session" ++ sId ++ ".json"


-- the main function executed by every Session
sessionMainLoop :: String -> IO ()
sessionMainLoop sId = forever $ do
    threadDelay 1000000
    mSession <- assembleSession sId
    case mSession of
        Just session -> putStrLn $ sId ++ " session: " ++ (show session)
        Nothing -> do
            putStrLn "err: Couldn't read the session file!"
            return ()


assembleSession :: String -> IO (Maybe Session)
assembleSession sId = do
    let sessionFileName = assembleSessionFilePath sId
    putStrLn $ "Reading session from file " ++ sessionFileName
    mPNodes <- maybeGetPullNodes sessionFileName
    case mPNodes of
        Just pNodes ->
                return $ Just Session { ssId = sId, ssPullNodes = pNodes}
        Nothing ->
                return Nothing