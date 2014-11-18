
-- solveRPN :: (Num a, Read a) => String -> a
-- solveRPN input = head . foldl foldingFunc [] (words input)
--     where   foldingFunc (x:y:ys) "*" = (x*y):ys
--             foldingFunc (x:y:ys) "+" = (x+y):ys
--             foldingFunc (x:y:ys) "-" = (x-y):ys
--             foldingFunc xs number    = read number:xs



--
-- LHR to London problem

import Data.List

-- each section is an algebraic data type, composed of three integers (3 roads)
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
-- a type synonim, saying that RoadSystem is a list of sections
type RoadSystem = [Section]


data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]



heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30,
                    Section 5 90 20,
                    Section 40 2 25,
                    Section 10 8 0]


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,c):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B,a):pathB
                        else (C,c):(A,a):pathA
        in (newPathToA, newPathToB)


groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then bestAPath
            else bestBPath

main = do
    let b = optimalPath heathrowToLondon
    print b