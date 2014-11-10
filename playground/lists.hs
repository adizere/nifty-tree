import Data.Char

-- a = [1,2,3,4,6,7]
-- a = "yello"

-- main = print ('j':a ++ "world")

b = [x*2 | x<-[1,2..30], (x*4) <= 100, x `mod` 7 == 3]

boomBang xs = [ if x < 10 then " BOOM!" else " BANG!" | x <- xs, odd x ]

--
-- Converting a [Char] to uppercase
--

toUpperCase :: [Char] -> [Char]
toUpperCase s = [ convert c | c <- s ]

convert :: Char -> Char
convert c
    | isLowerCase c = Data.Char.toUpper c
    | otherwise = c

isLowerCase :: Char -> Bool
isLowerCase c
    | c `elem` ['a'..'z']   = True
    | otherwise             = False

--
-- Remove non uppercase characters
--
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase s = [ c | c <- s, c `elem` ['A'..'Z'] ]


main = print( removeNonUpperCase "Seriously?!" )