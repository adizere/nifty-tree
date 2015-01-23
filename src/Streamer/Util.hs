module Streamer.Util where


import Data.List        -- for folds
import Data.Maybe       -- for listToMaybe
import System.Random    -- for RandomGen
import qualified Data.Map as DM


-- | Returns the last n elements of a list.
lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)


-- | Parses a String into Just a or into Nothing
--
-- For example, to obtain an Int from a stdin line,
--
-- > line <- getLine
-- > let res = (maybeRead line :: Maybe Int)
--
-- > case res of
-- >    Just v  -> *do something with the Int v*
-- >    Nothing -> ...
maybeRead :: Read a => String -> Maybe a
maybeRead val = fmap fst . listToMaybe . reads $ val


-- |  /O(n*log n)/. Returns a shuffled list of Int of a given length. All the elements are from
-- the interval [1, length], where length is the specified length.
--
-- For example, in the following
-- > sList <- getShuffledList 10
--
-- @sList@ will contain elements from the interval [1..10] in a random order.
--
-- Note: This function uses the 'Streamer.Util.shuffle' for shuffling the
-- elements.
getShuffledList :: Int -> IO [Int]
getShuffledList nCount = do
    gen <- getStdGen
    let (res, genU) = shuffle gen $ [1..nCount]
    setStdGen genU
    return res


-- | /O(n*log n)/. Shuffles the elements of a list in a random order. Original
-- source code at: https://www.haskell.org/haskellwiki/Random_shuffle
--
-- For example,
-- > g <- getStdGen
-- > let (l, gen) = shuffle g $ [1..100]
-- This will shuffle the 100 elements given as a second argument.
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
    toElems $ foldl _shuffleStep (initial (head l) gen) (numerate (tail l))
    where
        toElems (x, y) = (DM.elems x, y)
        numerate = zip [1..]
        initial x g = (DM.singleton 0 x, g)


-- | Helper function for the 'shuffle' algorithm.
_shuffleStep :: RandomGen g => (DM.Map Int a, g) -> (Int, a) -> (DM.Map Int a, g)
_shuffleStep (m, gen) (i, x) = ((DM.insert j x . DM.insert i (m DM.! j)) m, gen')
    where
        (j, gen') = randomR (0, i) gen


-- | Transforms a list of numbers such that they are all unique and strictly
-- smaller than a given number.
--
-- Some examples:
-- > uniqueWrappedNumbers [0,1,0]       3 [] = [2,1,0]
-- > uniqueWrappedNumbers [1,1,1,1]     4 [] = [0,3,2,1]
-- > uniqueWrappedNumbers [0,0,0,1,1,1] 5 [] = [5..0]
-- > uniqueWrappedNumbers [100,200,300] 3 [] = [0,2,1]
uniqueWrappedNumbers ::
    [Int]       -- ^ Initial list of numbers
    -> Int      -- ^ Open upper limit, for wrapping the numbers
    -> [Int]    -- ^ Accumulator for the final list of numbers
    -> [Int]    -- ^ Final list of numbers
uniqueWrappedNumbers []     _ a = a
uniqueWrappedNumbers (x:xs) l a =
    -- Basic check that we can accomplish this computation
    if (length xs) + 1 > l
        then error "Cannot transform list: the limit is too small!"
        else
            if (tMod x) `elem` a
                then uniqueWrappedNumbers xs l ((searchNextX x):a)
                else uniqueWrappedNumbers xs l ((tMod x):a)
    where
        searchNextX nx =
            if (tMod $ nx+1) `elem` a
                    then searchNextX (nx+1)
                    else tMod $ nx+1
        tMod t = t `mod` l