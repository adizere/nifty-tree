collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
    | even n    = n:collatzChain (n `div` 2)
    | odd n     = n:collatzChain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatzChain [1..200]))
    where isLong xs = length xs > 15


main = print (numLongChains)