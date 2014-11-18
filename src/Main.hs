module Main where

import Streamer.Session
import Streamer.Parser
import Streamer.Types

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    toParse <- BL.readFile "input.json"
