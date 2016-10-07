doubleAndSum :: [Int] -> Int
{- we're using foldr not foldl because we want to double every other number from the right, if we used foldl we would have to reverse the list then double every other.
-}
{-it doubles every other number from the right and calculates the sum of each digit.
-}
{-foldr it folds the list from the right
the function takes as its argument the accumulator and the head of the list which will 
produce a new accumulator and the function will take as its argument the new accumulator and
the next head of the list
fst extract the first component of the pairt-}

doubleAndSum = fst . 
               foldr (\i (acc, even) -> (acc + nextStep even i, not even)) (0, False)
	where 
	  nextStep even i
	    | even      = (uncurry (+) . (`divMod` 10) . (*2)) i
	    | otherwise = i 
{-it takes a credit card number..
-}
myLuhn :: Int -> Bool
myLuhn = (0 ==) . (`mod` 10) . doubleAndSum . (map (read . (: ""))) . show

testCC :: [Bool]
testCC = map myLuhn [49927398716, 49927398717, 1234567812345678, 1234567812345670]
{- => [True, False, False, True]
-}