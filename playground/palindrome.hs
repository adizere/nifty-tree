--
-- this version takes each line individually
--
isPalindrome :: String -> String
isPalindrome input
    | input == reverse input    = "palindrome!\n"
    | otherwise                 = "boo\n"


-- main = do
--     line <- getLine
--     putStr $ isPalindrome line
--     main


--
-- this version takes everything in one go..
--
respondPalindrome :: String -> String
respondPalindrome input =
    -- unlines . map (\xs -> if xs == reverse xs then "palindrome" else "boo!!") . lines $ input
    unlines . map (\xs -> isPalindrome xs) . lines $ input


main = interact respondPalindrome