module Streamer.Util where

import Data.List
import Data.Maybe -- for listToMaybe


-- returns the last n elements of list xs
lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)


-- parses a String into Just a or into Nothing
-- usage: to obtain a Int from a stdin line do this
--      line <- getLine
--      let res = (maybeRead line :: Maybe Int)
maybeRead :: Read a => String -> Maybe a
maybeRead val = fmap fst . listToMaybe . reads $ val