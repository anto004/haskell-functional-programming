import Data.List (isPrefixOf)
mapping = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),
           ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]
toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic rest
    where (num, rest) = oneStep2 str
{- 
Note: could also be written as follows.
toArabic str = 
    let (num, rest) = oneStep str 
    in num + toArabic rest
-}

oneStep :: String  -> (Int, String)
oneStep str =
   head [(num, drop (length roman) str) | 
                (roman,num) <- mapping, roman `isPrefixOf` str]    

testCases = ["MCMXC", "MMVIII", "MDCLXVI"]
test = zip testCases (map toArabic testCases) 

check2char str 
			| str !! 0 == 'C' && str !! 1 == 'M' = "CM"
			| str !! 0 == 'C' && str !! 1 == 'D' = "CD"
			| str !! 0 == 'X' && str !! 1 == 'C' = "XC"
			| str !! 0 == 'X' && str !! 1 == 'L' = "XL"
			| str !! 0 == 'I' && str !! 1 == 'X' = "IX"
			| str !! 0 == 'I' && str !! 1 == 'V' = "IV"
			| otherwise = " "

oneStep2 str
			| twoChar == "CM" = (900, tail (tail str))
			| twoChar == "CD" = (400, tail (tail str))
			| twoChar == "XC" = (90, tail (tail str))
			| twoChar == "XL" = (40, tail (tail str))
			| twoChar == "IX" = (9, tail (tail str))
			| twoChar == "IV" = (4, tail (tail str))
			| firstChar == 'M' = (1000, tail str)
			| firstChar == 'C' = (100, tail str)
			| firstChar == 'D' = (500, tail str)
			| firstChar == 'L' = (50, tail str)
			| firstChar == 'X' = (10, tail str)
			| firstChar == 'V' = (5, tail str)
			| firstChar == 'I' = (1, tail str)
			where
				twoChar = check2char str
				firstChar = str !! 0
