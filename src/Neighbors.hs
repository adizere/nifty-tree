module Neighbors
(   neighborsInputFile
,   getCyclesCount
) where


import System.IO


neighborsInputFile :: String
neighborsInputFile = "input.txt"


getCyclesCount :: IO String
getCyclesCount = do
    x <- System.IO.readFile neighborsInputFile
    return . readIgnoringComments $ lines x


readIgnoringComments :: [String] -> String
readIgnoringComments (x:xs)
    | length x == 0 =
            readIgnoringComments xs
    | head x == '#' =
            readIgnoringComments xs
    | otherwise     =
            x