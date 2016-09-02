odds = [1,2..10]

smallDivs n = [d | d <- takeWhile (<= n `div` 2) odds, n `mod` d == 0]

test = [x*2 | x <- [1..15], x*2 >= 12]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 