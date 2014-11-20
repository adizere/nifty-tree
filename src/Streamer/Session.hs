module Streamer.Session where

import Streamer.Types
import Streamer.SessionManager
import Streamer.Util
import Data.Char
import System.Directory


-- directory with sessions
sessionDirectory :: FilePath
sessionDirectory = "."


data Session = Session
    { id        :: Int
    , pullNodes :: PullNodesList
    , manager   :: SessionManager
    }  deriving (Eq, Show)


startSession :: Session -> Bool
startSession _ =
    True


printAvailableSessions :: IO ()
printAvailableSessions = do
    putStrLn "Available sessions are.."
    -- print a
    a <- getSessionsFromDirectory sessionDirectory
    print a


getSessionsFromDirectory :: FilePath -> IO [String]
getSessionsFromDirectory path = do
    allFiles <- getDirectoryContents sessionDirectory
    let sessionFiles = [ file | file <- allFiles, isSessionFile file ]
    -- use splitAt to cut the ".json" part
    let sessionIds = [ drop 7 file | file <- sessionFiles ]
    return sessionIds


isSessionFile :: String -> Bool
isSessionFile fileName =
    -- the file has a json extension
    lastN' 5 fileName == ".json"
    && take 7 fileName == "session"
    && isDigit (last . take 8 $ fileName)


getSessionIdFromFileName :: String -> String
getSessionIdFromFileName = undefined