module Main where


import Streamer.Session (getAllSessions, joinSession, SessionActor, startSessionActor)
import Streamer.Types
import Streamer.Parser()
import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy as BL


-- 1 second sleep time
sleepTimeMs :: Int
sleepTimeMs = 1000000


main :: IO ()
main = do
    mainLoop [] [] ""


-- handles Session selection and Session joining
--
mainLoop :: [String] -> [SessionActor] -> String -> IO ()
mainLoop runningSessionIds runningThreads newThread = do
    allSessionIds <- getAllSessions
    putStrLn $ show runningThreads
    if length newThread > 0
        then do
            newActor <- doJoinSession newThread
            mainLoop (newThread:runningSessionIds)
                     (newActor:runningThreads)
                     ""
        else do
            maybeId <- chooseNewSessionId allSessionIds runningSessionIds
            let (can, msg) = canJoinSession maybeId runningSessionIds allSessionIds
            case can of
                True  -> mainLoop
                                runningSessionIds
                                runningThreads
                                maybeId
                False -> putStrLn msg
            mainLoop runningSessionIds runningThreads ""


-- presents the Running Sessions and All Sessions information
-- allows to choose a new Session to join
chooseNewSessionId :: [String] -> [String] -> IO String
chooseNewSessionId aIds rIds = do
    putStrLn $ "---\nRunning sessions are: " ++ (intercalate ", " rIds)
    putStrLn $ "The available sessions are: " ++ (intercalate ", " aIds)
    putStrLn "Join a session? Which one?"
    readSessionId


-- checks if we can Join a new Session
canJoinSession :: String -> [String] -> [String] -> (Bool, String)
canJoinSession sId runningIds allIds =
    if sId `elem` allIds
        then if sId `elem` runningIds
                then (False, "err: Session '"
                              ++ sId
                              ++ "' is already running!")
                else (True, "")
        else (False, "err: The session with id '"
                        ++ sId
                        ++ "' does not exist!")


doJoinSession :: String -> IO SessionActor
doJoinSession sid = do
    a <- startSessionActor sid
    return a


readSessionId :: IO String
readSessionId = do
    line <- getLine
    if length line == 0
        then readSessionId
        else return . head . words $ line


-- to be (re)moved
_dummyParsePullNode :: IO ()
_dummyParsePullNode = do
    toParse <- BL.readFile "input.json"
    let a = decode $ toParse :: Maybe PullNode
    print a