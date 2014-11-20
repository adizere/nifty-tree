module Main where

import Streamer.Session
import Streamer.Parser
import Streamer.Types
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Control.Concurrent


-- 1 second sleep time
sleepTimeMs = 1000000


main :: IO ()
main = do
    printAvailableSessions
    threadDelay sleepTimeMs
    main


_dummyParsePullNode = do
    toParse <- BL.readFile "input.json"
    let a = decode $ toParse :: Maybe PullNode
    print a