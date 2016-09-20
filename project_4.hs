validCC :: Integer
validCC = 5424181210750423

toDigits :: Integer -> [Integer]
toDigits x
	| x <= 0 = []
	| otherwise = toDigits (div x 10) ++ [mod x 10]

toDigitsReverse :: [Integer] -> [Integer]
toDigitsReverse xr = reverse xr

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x : (2*y) : doubleEveryOther xs 

sum' :: Integer -> Integer
sum' x = (x `mod` 10) + (x `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum' x
sumDigits (x:xs) = sum' x + sumDigits xs
	

validate :: Integer -> Bool
validate x
	| s `mod` 10 == 0 = True
	| otherwise = False
	where 
		y = toDigits x
		z = toDigitsReverse y
		d = doubleEveryOther z
		s = sumDigits d



