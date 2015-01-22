module Main where


import Streamer.Session ( getAllSessions
                        , getSessionIdsFromSessionHandles
                        , startSession
                        , shMVar
                        , SessionHandle )

import System.IO          (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Environment (getArgs)
import Data.List
import Control.Concurrent   -- for MVar control
import Control.Monad        -- for filterM


-- 1 second sleep time
sleepTimeMs :: Int
sleepTimeMs = 1000000


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    argz <- getArgs
    if ( (length argz >= 1 ) && (head argz) == "-a")
        then autoStream
        else interactiveStream [] ""


-- | Automatically starts to try and stream data, from the first session found.
autoStream :: IO ()
autoStream = do
    sId <- doStartSession ""  -- starts session described in file "session.json"
    waitForAutoSession sId    -- returns when the session finishes
    return ()


-- | Waits until a given session finishes and then returns.
waitForAutoSession :: SessionHandle -> IO ()
waitForAutoSession sId = do
    stillRunning <- checkRunningSessions [sId]
    if length stillRunning > 0
        then
            threadDelay 1000000 >> waitForAutoSession sId
        else return ()


--
-- takes care of Session selection and Session starting
--
interactiveStream :: [SessionHandle] -> String -> IO ()
interactiveStream sessionHandles startNewSessionId = do
    runningSessionHandles <- checkRunningSessions sessionHandles
    putStrLn $ "The running sessions are: " ++ (show runningSessionHandles)
    allSessionIds <- getAllSessions
    let runningSessionIds =
            getSessionIdsFromSessionHandles runningSessionHandles
    -- checks if it should start a new Session
    if length startNewSessionId > 0
        then do
            -- fires up another session, & calls itself with revised arguments
            newSession <- doStartSession startNewSessionId
            interactiveStream (newSession:runningSessionHandles) ""
        else do
            -- allows the user to choose a Session to start
            maybeId <- chooseNewSessionId allSessionIds runningSessionIds
            -- validate the choice
            let (can, msg) = canStartSession
                                maybeId
                                runningSessionIds
                                allSessionIds
            case can of
                -- if the choice is valid, then the function calls itself with
                -- the chosen Session Id
                True  -> interactiveStream runningSessionHandles maybeId
                False -> putStrLn msg
            -- calls itself in case of invalid SessionId choice
            interactiveStream runningSessionHandles ""


-- presents the runningSessions and allSessions information
-- allows the user to choose a new Session to start
chooseNewSessionId :: [String] -> [String] -> IO String
chooseNewSessionId aIds rIds = do
    putStrLn $ "---\nRunning sessions are: " ++ (intercalate ", " rIds)
    putStrLn $ "The available sessions are: " ++ (intercalate ", " aIds)
    putStrLn "Start a session? Which one?"
    readSessionId


-- given the list with runningSessionIds, the list with allSessionIds, and
-- a SessionId, it checks if the SessionId is valid
canStartSession :: String -> [String] -> [String] -> (Bool, String)
canStartSession sId runningIds allIds =
    if sId `elem` allIds
        then if sId `elem` runningIds
                then (False, "err: Session '"
                              ++ sId
                              ++ "' is already running!")
                else (True, "")
        else (False, "err: The session with id '"
                        ++ sId
                        ++ "' does not exist!")


-- starts a Session and returns the SessionHandle for it
doStartSession :: String -> IO SessionHandle
doStartSession sid = do
    a <- startSession sid
    return a

-- waits for input from the user and returns the first word from stdin
readSessionId :: IO String
readSessionId = do
    line <- getLine
    if length line == 0
        then readSessionId
        else return . head . words $ line


-- takes a list of SessionHandles
-- checks if any of them finished (by looking at the shMVar field)
-- removes any finished SessionHandle, and returns the updated list
checkRunningSessions :: [SessionHandle] -> IO [SessionHandle]
checkRunningSessions handles = do
    -- filter the list of handles
    -- only keep those handles whose shMVar is empty
    filterM (\h -> isEmptyMVar (shMVar h)) handles