quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x ]
    in smallerSorted ++ [x] ++ biggerSorted


main = print (quicksort "the quick brown fox jumps over the lazy dog"  )