-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage a = print a
-- main = printAMessage 50 

-- Write division here
division :: Double -> Double -> Maybe Double
division x y = Just ( x / y )
-- main = print ( division 10 2 )

-- Write factorial here
factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Write factList here
factList :: Integer -> [Integer]
factList n = map factorial [1..n]
-- main = print ( factList 6 )

-- Write merge here
maxValue :: Int -> Int -> Int
maxValue a b
    | a < b     = b
    | otherwise = a

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | maxValue x y == x = y : merge (x:xs) ys
    | otherwise         = x : merge xs (y:ys)

main = print (merge [1, 3, 5, 7] [2, 4, 6, 8])
