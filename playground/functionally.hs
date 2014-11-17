
solveRPN :: (Num a, Read a) => String -> a
-- solveRPN _ =>
solveRPN input = head . foldl foldingFunc [] . words input
    where   foldingFunc (x:y:ys) "*" = (x*y):ys
            foldingFunc (x:y:ys) "+" = (x+y):ys
            foldingFunc (x:y:ys) "-" = (x-y):ys
            foldingFunc xs number    = read number:xs



--
-- LHR to London problem

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