module Neighbors
(   neighborsInputFile
,   getCyclesCount
) where

import System.IO

neighborsInputFile :: [Char]
neighborsInputFile = "input.txt"


getCyclesCount :: String -> IO String
getCyclesCount fileName = do
    x <- System.IO.readFile fileName
    let allLines    = lines x
    let firstLine   = readIgnoringComments (lines x)
    putStrLn firstLine
    -- firstLine


readIgnoringComments :: [String] -> String
readIgnoringComments (x:xs)
    | head x == '#'     = readIgnoringComments xs
    | otherwise         = x