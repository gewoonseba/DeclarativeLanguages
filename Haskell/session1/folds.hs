--Session 1: Part 2: Folds
--2.1 Your own implementation

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

fold :: (a -> b ->b) -> b -> [a] -> b
fold _ n [] = n
fold f n (x:xs) = f x (fold f n xs)
--fold = foldr

--2.2 associativity

readInBase :: Int -> [Int] -> Int
readInBase b = foldl (\acc x -> acc*b+x) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr(\x acc -> f x : y) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' _ [] = []
myMap' f (x:xs) = f x : myMap' f xs
