odds = [3,5..6000]

smallDivs n = [d | d <- takeWhile (<= n `div` 2) odds, n `mod` d == 0]

primes = 2:[p | p <- odds, smallDivs p == []]

isAPrime x = elem x primes

squares = [x * x | x <- [1..]]

isASquare x = elem x (takeWhile (<= x)squares)

test g = [p | p <- primes, isASquare ((g - p) `div` 2)]

gConjucture = [g | g <- odds, not(isAPrime g), null(test g)]


