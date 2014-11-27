import System.Random
import Data.List


rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))


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


main = do
    a <- getRandomNumbers (10, 100) 50
    putStrLn (show a)