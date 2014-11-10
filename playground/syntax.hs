lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number slevin"
lucky x = "Nope!"


enumerate x = [ lucky c | c <- x ]

--
-- Tuple manipulation
--
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Triples..
first :: (Num a) => (a, a, a) -> a
first (x, _, _) = x

second :: (Num a) => (a, a, a) -> a
second (_, y, _) = y

third :: (Num a) => (a, a, a) -> a
third (_, _, z) = z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


--
-- Guards
--
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight!"
    | bmi <= 25 = "Normal.. average"
    | bmi <= 30 = "Fattie huehue"
    | otherwise = "World-class whale"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

-- Function called infix-style
compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

main = print ( 3 `compare` 2 )