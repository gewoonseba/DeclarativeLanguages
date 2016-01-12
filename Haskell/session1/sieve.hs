--Session 1 -5 Prime numbers

sieve :: Int -> [Int]
sieve n
  | n <= 0 = []
  | otherwise = sieve' n [2..n]

sieve' :: Int -> [Int] -> [Int]
sieve' _ [] = []
sieve' n (x:xs)
  | x > n = []
  | otherwise = x : sieve' n (removeMultiples x xs)

sieveSquare :: Int -> [Int]
sieveSquare n
  | n <= 0 = []
  | otherwise = sieveSquare' n [2..n]

sieveSquare' :: Int -> [Int] -> [Int]
sieveSquare' _ [] = []
sieveSquare' n (x:xs)
  | x > squareRoot n = xs
  | otherwise = x : sieveSquare' n (removeMultiples x xs)

floorSquare :: Int -> Int
floorSquare = floorMono . sqrtMono . i2d

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n xs = filter (\x -> x `mod`n /= 0) xs

sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor
