module Streamer.Session
( sessionDirectory
, startSession
, getAllSessions
, getSessionIdsFromSessionHandles
, shMVar
, SessionHandle
) where


import Streamer.Parents
import Streamer.SessionManager

import Data.Char
import System.Directory
import Control.Concurrent
import Control.Exception.Base (finally)


--------------------------------------------------------------------------------
--- public functions and data types
---

-- directory with session file descriptors
sessionDirectory :: FilePath
sessionDirectory = "/opt/streamer"


-- each SessionHandle describes a running session
data SessionHandle = SessionHandle
    { shSessionId   :: String       -- Session ID running with this handle;
    , shThreadId    :: ThreadId     -- Thread which contains this session;
    , shMVar        :: MVar (Int)   -- shMVar: If empty, the session is running,
                                    -- otherwise, the session finished;
    } deriving (Eq)


-- | SessionHandle is a Show instance.
instance Show SessionHandle where
    show SessionHandle { shSessionId = r , shThreadId = t } =
        "SessionHandle (id=" ++ (show r) ++ "," ++ (show t) ++ ")"


-- | Sparks a new thread, which will handle exclusively a given session.
-- Expects as parameter the id of a session.
-- When the thread finishes, it will fill the shMVar, signaling that it is done.
startSession :: String -> IO SessionHandle
startSession sId = do
    m <- newEmptyMVar :: IO (MVar Int)
    tId <- forkIO (finally (runSession sId) (putMVar m 1))
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
--- Private functions
---


-- | The main function executed for every running Session.
runSession :: String -> IO ()
runSession sId = do
    threadDelay 1000000
    mSManager <- getSessionManager sId
    case mSManager of
        Just sm     -> do
            putStrLn $ sId ++ " session: " ++ (show sm)
            startSessionManager sm
            -- runSession sId
        Nothing     -> putStrLn "err: Couldn't read the session file!"


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
getSessionManager :: String -> IO (Maybe SessionManager)
getSessionManager sId = do
    let sessionFileName = assembleSessionFilePath sId
    putStrLn $ "Reading session from file " ++ sessionFileName
    mPNodes <- maybeSelectParents sessionFileName
    case mPNodes of
        Just pNodes -> return $ Just $ SessionManager { smParents = pNodes
                                                      , smFramesSeqNr = []}
        Nothing     -> return Nothing