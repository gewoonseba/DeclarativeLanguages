--Session 2 - 4 Chaining

applyAll :: [a -> a] -> a -> a
applyAll [] = id
applyAll (f:fs) = f . applyAll fs

--OR

applyAll' :: [a -> a] -> a -> a
applyAll' = foldl (.) id

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 _ = id
applyTimes n f = f . applyTimes (n-1) f

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs _ [] = []
applyMultipleFuncs a (f:fs) = f a : applyMultipleFuncs a fs
