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


--------------------------------------------------------------------------------
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


-- | SessionHandle is a Show instance.
instance Show SessionHandle where
    show SessionHandle { shSessionId = r , shThreadId = t } =
        "SessionHandle (id=" ++ (show r) ++ "," ++ (show t) ++ ")"


-- | Sparks a new thread, which will handle exclusively a given session.
-- Expects as parameter the id of a session.
startSession :: String -> IO SessionHandle
startSession sId = do
    m <- newEmptyMVar
    tId <- forkIO (finally (sessionMainLoop sId) (putMVar m 1))
    return SessionHandle { shSessionId  = sId
                         , shThreadId   = tId
                         , shMVar       = m }


-- | Searches a directory where sessions are expected to reside and returns
-- all the ids of the found sessions.
getAllSessions :: IO [String]
getAllSessions = do
    getSessionsFromDirectory sessionDirectory


-- | Extracts the session id from every SessionHandle in a list. Returns all the
-- extracted ids.
getSessionIdsFromSessionHandles :: [SessionHandle] -> [String]
getSessionIdsFromSessionHandles iSessions =
    -- use the accessor function to extract the sessionId from each iSession
    [ shSessionId session | session <- iSessions ]


--------------------------------------------------------------------------------
--- private functions and data types
---


data Session = Session
    { ssId          :: String
    , ssPullNodes   :: PullNodesList
    , ssManager     :: SessionManager
    } deriving (Eq, Show)


-- | The main function executed for every running Session.
sessionMainLoop :: String -> IO ()
sessionMainLoop sId = forever $ do
    threadDelay 1000000
    mSession <- assembleSession sId
    case mSession of
        Just ss     -> do
            putStrLn $ sId ++ " session: " ++ (show ss)
            startSessionManager $ ssManager ss
        Nothing     -> do
            putStrLn "err: Couldn't read the session file!"
            return ()


-- | Searches a given directory and returns all the ids of the found sessions.
getSessionsFromDirectory :: FilePath -> IO [String]
getSessionsFromDirectory path = do
        allFiles <- getDirectoryContents path
        let ids = map getSessionIdFromFileName allFiles
        return [ sId | sId <- ids, length sId > 0]


-- | Extracts the id of the session from a given file name.
-- Expects a string (file name) of the form: "sessionX.json", where X is a
-- string of digits, representing the id.
--
-- For example,
-- >    getSessionIdFromFileName "session456.json"
-- >    getSessionIdFromFileName "session.json"
-- will return "456" and "", respectively.
getSessionIdFromFileName :: String -> String
getSessionIdFromFileName fileName
    | length fileName <= 12         = ""
    | take 7 fileName /= "session"  = ""
    | otherwise                     =
        [ x | x <- snd . splitAt 7 $ fileName, isDigit x]


-- | Constructs the name of the file where information about a session is
-- expected to reside.
-- Takes as argument a string representing the id of a session.
-- Returns the file name where that session is described (.json file).
assembleSessionFilePath :: String -> String
assembleSessionFilePath sId =
    sessionDirectory ++ "/" ++ "session" ++ sId ++ ".json"


-- | Constructs a Session object for a given session id.
-- This function
assembleSession :: String -> IO (Maybe Session)
assembleSession sId = do
    let sessionFileName = assembleSessionFilePath sId
    putStrLn $ "Reading session from file " ++ sessionFileName
    mPNodes <- maybeGetPullNodes sessionFileName
    case mPNodes of
        Just pNodes -> do
                let mgr = SessionManager {smPullNodes = pNodes, smFrames = []}
                return $ Just Session { ssId = sId
                                      , ssPullNodes = pNodes
                                      , ssManager = mgr}
        Nothing ->
                return Nothing

