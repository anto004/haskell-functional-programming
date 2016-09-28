a = curry(\(x,y)-> x * y)

digitToInt :: Char -> Int
digitToInt = read . (:[])

b = map (uncurry (+) . (`divMod` 10)) . zipWith (*) (cycle [1,2]) . map digitToInt . reverse