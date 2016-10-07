
import Data.Char (digitToInt)
luhn = (0 ==) . (`mod` 10) . sum . map (uncurry (+) . (`divMod` 10)) .
       zipWith (*) (cycle [1,2]) . map digitToInt . reverse



toDigits :: String -> [Integer]
toDigits = map (read . (:[]))

{-
myLuhn :: Integer -> Bool
myLuhn = (0 ==) . (`mod` 10) . sum . concat . map (toDigits . show) .   
        zipWith (*) (cycle [1,2]) . (toDigits . reverse . show)
 -}

revIntegers:: Integer -> [Integer]
revIntegers = toDigits . reverse . show

alternateDouble:: [Integer] -> [Integer]
alternateDouble = zipWith (*) (cycle [1, 2]) 

sumOfDigits:: [Integer] -> Integer
sumOfDigits = sum . concat . map (toDigits . show)

myLuhn':: Integer -> Bool
myLuhn' = (0 ==) . (`mod` 10) . sumOfDigits . alternateDouble . revIntegers