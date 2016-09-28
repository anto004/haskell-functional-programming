multThree :: (Num a) => a -> a -> a -> a 
multThree x y z = x * y * z
a = multThree 3
b = a 4

c = max 4

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100 

testZipWith =  zipWith (flip div) [2,2..] [10,8,6,4,2] 

division :: (Integral a )=> a -> a
division x = div x 2

myDigitToInt :: Char -> Int
myDigitToInt = read . (:[])