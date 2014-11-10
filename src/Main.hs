module Main where


import Neighbors


main = do
    x <- Neighbors.getCyclesCount
    putStrLn x