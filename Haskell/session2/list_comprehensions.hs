--Session 2 -3 List comprehensions

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [ x | x <- xs, p x]

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]

bind :: [a] -> (a -> [b]) -> [b]
bind m f = concat (map f m)

lc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1 f p = map f . filter p

lc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2 f as bf p = bind as $ \a ->
                bind (filter p (bf a)) $ \b -> [f a b]

lc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3 f n = bind (filter even [1..n]) $ \a ->
          bind [a..n] $ \b ->
          bind (filter (\c -> a ^ 2 + b ^ 2 == c ^ 2) [b..n]) $ \c ->
          [f a b c] 
