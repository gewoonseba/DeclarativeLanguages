-- Sebastian Stoelen
-- r
module Factory where
import Data.List (sort)

--Exercise 1
data Station a b = Machine [(a, Int)] b | Combine [Station a b]

machine :: [(a, Int)] -> b -> Station a b
machine = Machine

combine :: [Station a b] -> Station a b
combine = Combine

-- Exercise 2
trivial :: (Bounded a,Enum a) => Station a a
trivial = Combine $ map (\a -> Machine [(a,1)] a) [minBound..maxBound]

--Exercise 3
type Resources a = [(a, Int)]

startResources :: Resources a
startResources = []

amount :: Ord a => Resources a -> a -> Int
amount rs key = head $ [ b | (a,b) <- rs, a == key] ++ [0]

insert :: Ord a => Resources a -> a -> Resources a
insert rs key = (key, amount rs key + 1) : [(a, b) | (a, b) <- rs, a /= key]

--Exercise 4
run :: Ord a => [(a,Int)] -> b -> Resources a -> [a] -> ([a],[b])
run = undefined

--Exercise 5
runStation :: Ord a => Station a b -> [a] -> ([a],[b])
runStation = undefined
