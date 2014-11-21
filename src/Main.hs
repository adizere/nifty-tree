module Main where


import Streamer.Session (getAllSessions, joinSession)
import Streamer.Util
import Streamer.Parser
import Streamer.Types
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.List
import Control.Monad        -- "forever"
import qualified Control.Exception as CE


-- 1 second sleep time
sleepTimeMs = 1000000


main :: IO ()
main = do
    mainLoop []


mainLoop :: [String] -> IO ()
mainLoop runningSessions = do
    putStrLn $ "---\nRunning sessions are: " ++ (show runningSessions)
    ids <- getAllSessions
    putStr "The available sessions are: "
    putStrLn $ intercalate ", " ids
    putStrLn "Join a session? Which one?"
    maybeId <- readSessionId
    if maybeId `elem` ids
        then if maybeId `elem` runningSessions
                then do putStrLn $ "err: Session '"
                              ++ maybeId
                              ++ "' is already running!"
                        mainLoop runningSessions
                else do doJoinSession maybeId
                        mainLoop $ maybeId:runningSessions
        else do putStrLn $ "err: The supplied string '"
                        ++ maybeId
                        ++ "' is not a valid session id!"
                mainLoop runningSessions


doJoinSession :: String -> IO ()
doJoinSession sid =
    joinSession sid


readSessionId :: IO String
readSessionId = do
    line <- getLine
    if length line == 0
        then readSessionId
        else return . head . words $ line


_dummyParsePullNode = do
    toParse <- BL.readFile "input.json"
    let a = decode $ toParse :: Maybe PullNode
    print a