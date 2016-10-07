myDigitToInt :: Char -> Int
myDigitToInt = read . (:[]) -- The type tells read to produce an Int.

{-
myDigitToInt could have been defined using a parameter
myDigitToInt c = read [c]
That’s the same as
myDigitToInt c = read (c : [])
That’s the same as
myDigitToInt c = (read . (:[])) c
That’s the same as
myDigitToInt = read . (:[])
-}

{- the credit card number is entered as a string so
toDigits converts them to Numbers
-}
toDigits :: String -> [Int]
toDigits = map myDigitToInt 

-- cycle12 :: [Int]
-- cycle12 = cycle [1,2]

{-pairs takes a credit card number and pair it with 1's or 2's .
what we want is starting from the right the 
first cc number remains as it is so it will be  paired with 1
second cc number we want it doubled so it will be  paired with 2
-}
pairs :: [Int] -> [(Int, Int)]  --paired with one or two
pairs ds 
 | odd (length ds) = zip ds (cycle [1, 2]) --  each credit card number will be paired with each element from the list
 | otherwise       = zip ds (cycle [2, 1])


doubleEveryOther :: [Int] -> [Int] -- Could use zipWith in pairs instead.
doubleEveryOther = map (\(d, m) -> d * m) . pairs 

{- calculates the sum of each digit
what we want in this function is, the numbers that have been doubled
6->12
-}
sumDigits  :: [Int] -> Int
sumDigits = sum . concat . map (toDigits . show) -- Why not just sum?

{- it doubles every other number from the right and calulates the sum of each digit
-}
checkSum :: String -> Int
checkSum = sumDigits . doubleEveryOther . toDigits  

{-isValid checks if that credit card number is valid or not
-}
isValid :: String -> Bool
isValid n = checkSum n `mod` 10 == 0

{- takes a credit card number and doubles every other number from the right
calculates the sum of each digit and check if it is divisible by 10.
-}
testCC :: [Bool]
testCC = map isValid ["79927398713", "79927398714"]