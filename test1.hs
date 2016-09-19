oddsFrom3 = [3, 5 .. ]

listA = [3, 5 ..]
listB = [x | x <- [3, 5 ..]]
listC = [x | x <- [3, 5 .. 5999]]
listD = [x | x <- [3, 5 .. 6000]]
listE = [x | x <- [3, 5 ..], (x < 6000)]
listF = [x | x <- takeWhile (< 6000) [3, 5 ..] ]

primeDivisors n = [d | d <- takeWhile ((<= n) . (^2)) primes, n `mod` d == 0]

primes = 2 : [p | p <- oddsFrom3, null (primeDivisors p)]

isPrime g = g == head (dropWhile (< g) primes)

goldbachPairs g = [(p, k) | k <- takeWhile ((< g) . (*2) . (^2)) [1 .. ], 
                            p <- [g - 2 * k * k], isPrime p] 

goldbachFails = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g), 
                     null (goldbachPairs g) ]


iSqrt n = floor (sqrt (fromIntegral n))

isSquare :: Int -> Bool
isSquare n = (iSqrt n) ^ 2 == n 

goldbachPairs' g = [(p, iSqrt kSqr) | p <- takeWhile (< g) (tail primes),  
                                      kSqr <- [(g - p) `div` 2], 
                                      isSquare kSqr]

goldbachDiffs = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g),
								goldbachPairs g /= reverse (goldbachPairs' g)]
