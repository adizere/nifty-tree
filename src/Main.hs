module Main where


import Streamer.Session ( getAllSessions
                        , getSessionIdsFromSessionHandles
                        , startSession
                        , SessionHandle )
import Streamer.PullNodes
import Data.List


-- 1 second sleep time
sleepTimeMs :: Int
sleepTimeMs = 1000000


main :: IO ()
main = do
    mainLoop [] ""


--
-- takes care of Session selection and Session starting
--
mainLoop :: [SessionHandle] -> String -> IO ()
mainLoop sessionHandles startNewSessionId = do
    putStrLn $ "The running sessions are: " ++ (show sessionHandles)
    allSessionIds <- getAllSessions
    let runningSessionIds = getSessionIdsFromSessionHandles sessionHandles
    -- checks if it should start a new Session
    if length startNewSessionId > 0
        then do
            -- fires up another session, then calls itself with revised arguments
            newSession <- doStartSession startNewSessionId
            mainLoop (newSession:sessionHandles) ""
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
                True  -> mainLoop sessionHandles maybeId
                False -> putStrLn msg
            -- calls itself in case of invalid SessionId choice
            mainLoop sessionHandles ""


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


readSessionId :: IO String
readSessionId = do
    line <- getLine
    if length line == 0
        then readSessionId
        else return . head . words $ line