module Main where

-- import System.IO
-- import Neighbors

-- main = Neighbors.getCyclesCount

-- getCyclesCount :: String -> String
-- getCyclesCount fileName =
--     -- x <- System.IO.readFile fileName
--     -- print ([head l | l <- lines x, length l > 0 ])
--     readIgnoringComments (lines (System.IO.readFile fileName))


-- -- readIgnoringComments :: [String] -> String
-- readIgnoringComments (x:xs)
--     | length x == 0 =
--             readIgnoringComments xs
--     | head x == '#' =
--             readIgnoringComments xs
--     | otherwise     =
--             x



main = print "input.txt "