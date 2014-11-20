module Streamer.Session where

import Streamer.Types
import Streamer.SessionManager
import Streamer.Util
import Data.Char
import Data.List
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
    ids <- getSessionsFromDirectory sessionDirectory
    -- let b = foldl (\acc cid -> acc ++ "- " ++ cid ++ "\n" ) "" ids
    putStrLn $ intercalate ", " ids


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