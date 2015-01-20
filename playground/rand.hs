import System.Random
-- import qualified Data.List
import qualified Data.Map as DM


rollDice :: IO Int
rollDice = getStdRandom (randomR (10,30))


-- getRandoms :: (Int, Int) -> Int -> IO [Int]
-- getRandoms (lo, hi) count = do
--     g <- getStdGen
--     return . take count $ (randomRs (lo, hi) g)


-- boundedRandomNumber :: (Int, Int) -> IO Int
-- boundedRandomNumber (lo, hi) = do
--     getStdRandom (randomR (lo,hi))


getNonRepeatingRandoms :: (Int, Int) -> Int -> IO [Int]
getNonRepeatingRandoms (l, h) count = do
    g <- getStdGen
    return $ _genNRRandoms (l, h) count [] g


_genNRRandoms :: (Int, Int) -> Int -> [Int] -> StdGen -> [Int]
_genNRRandoms (_, _) 0 accum _ = accum
_genNRRandoms (lo, hi) cnt accum g =
    if rnd `elem` accum
        then _genNRRandoms (lo, hi) cnt accum gnr
        else _genNRRandoms (lo, hi) cntU accumU gnr
    where
        (rnd, gnr) = randomR (lo, hi) g
        cntU = cnt - 1
        accumU = rnd:accum



------------------------------------
-- Shuffling --
-- Sources:
-- https://www.haskell.org/haskellwiki/Random_shuffle
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
--

shuffleStep :: RandomGen g => (DM.Map Int a, g) -> (Int, a) -> (DM.Map Int a, g)
shuffleStep (m, gen) (i, x) = ((DM.insert j x . DM.insert i (m DM.! j)) m, gen')
    where
        (j, gen') = randomR (0, i) gen


shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
    toElems $ foldl shuffleStep (initial (head l) gen) (numerate (tail l))
    where
        toElems (x, y) = (DM.elems x, y)
        numerate = zip [1..]
        initial x gen = (DM.singleton 0 x, gen)


main = do
    -- a <- getNonRepeatingRandoms (1, 100) 10
    -- putStrLn $ (show a)
    -- g <- getStdGen
    -- let (l, gen) = shuffle g $ [1..100]
    -- putStrLn $ (show l)
    mapM_ (\r -> rollDice >>= \d -> putStrLn (show d)) [1..100]