--Haskell exercise session 1

--1 Hors d'Oeuvres

myLast :: [Int] -> Int
myLast [x] = x
myLast (x:xs) = myLast xs
--last

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n <= 0 = []
myRepeat n x = (x:myRepeat (n-1) x)
--replicate n x

flatten :: [[Int]] -> Int
flatten [] = []
flatten (x:xs) = x ++ flatten xs
--concat

range :: Int -> Int -> [Int]
range a b
    | a > b = []
    | ohterwise = (a: range nA b)
    where nA = a + 1
--[a..b]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples _ [] = []
removeMultiples n (x:xs)
    | x `mod` n == 0 = rest
    | otherwise = (x:rest)
    where rest = removeMultiples n xs
-- [x | x <- xs, x `mod`n \= 0]
--of: removeMultiples n = filter (\x -> x`mod`n /= 0)
