module Streamer.Util where


import Data.List        -- for folds
import Data.Maybe       -- for listToMaybe
import System.Random


-- returns the last n elements of list xs
lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)


-- parses a String into Just a or into Nothing
-- usage: to obtain a Int from a stdin line do this
--      line <- getLine
--      let res = (maybeRead line :: Maybe Int)
maybeRead :: Read a => String -> Maybe a
maybeRead val = fmap fst . listToMaybe . reads $ val


-- Generates multiple random numbers distributed within a closed interval
-- Takes as arguments the interval as a tuple and the count of rand numbers
-- that it should generate.
getRandomNumbers :: (Int, Int) -> Int -> IO [Int]
getRandomNumbers (lo, hi) count = do
    _getRandomNumbers (lo, hi) count []


_getRandomNumbers :: (Int, Int) -> Int -> [Int] -> IO [Int]
_getRandomNumbers (lo, hi) 0 accum = do return accum
_getRandomNumbers (lo, hi) cnt accum = do
    rnd <- boundedRandomNumber (lo, hi)
    let uCnt = cnt-1
    _getRandomNumbers (lo, hi) uCnt $ rnd:accum


boundedRandomNumber :: (Int, Int) -> IO Int
boundedRandomNumber (lo, hi) = do
    getStdRandom (randomR (lo,hi))