--Session 3 - 5 Monadic functions
import Control.Monad

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = m >>= \a ->
                   sequence ms >>= \as ->
                   return (a : as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence (map f as)

zipWith' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWith' f as bs = sequence (zipWith f as bs)

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n m = sequence (replicate n m)
