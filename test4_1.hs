{- Convert positive integet to list of digit -}
{- Input : 12345 o/p : [1, 2, 3, 4, 5 -}
toList :: Integer -> [Integer]
toList 0 = []
toList x = toList (x `div` 10) ++ [x `mod` 10]


{- Reverse the integer after converting it to list -}
-- toDigitsRev :: Integer -> [Integer]
toListReverse :: Integer -> [Integer]
toListReverse xs = (reverse . toList) xs

{- Double every second number  -}
doubleSecondEach :: [Integer] -> [Integer]
doubleSecondEach [] = []
doubleSecondEach (x:[]) = [x]
doubleSecondEach (x:y:xs) = x : (2*y) : doubleSecondEach xs 

{- Sum the digit in the number we are sure there is one digit number or two digit number -}
sum' :: Integer -> Integer
sum' x = (x `mod` 10) + (x `div` 10)

{- sum all the element in list -}
{- when a element is [13,5,6] we have to add like this: 1 + 3 + 5 + 6 -}
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum' x
sumDigits (x:xs) = sum' x + sumDigits xs

{- Validate the credit card number -}
validate :: Integer -> Bool
validate x = if (sumDigits $ doubleSecondEach $ toListReverse x ) `mod` 10 == 0 then True else False