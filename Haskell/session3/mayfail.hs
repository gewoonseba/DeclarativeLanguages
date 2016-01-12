
import Control.Applicative (Applicative(..))
import Control.Monad (liftM2, join, ap)

--Session 3 - 3 MayFail Monad

data MayFail e a = Error e | Result a
                  deriving Show

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp
          deriving Show

safeDiv :: Int -> Int -> MayFail String Int
safeDiv _ 0 = Error "Division by zero"
safeDiv a b = Result (a `div`b)

instance Functor (MayFail e) where
  fmap _ (Error e) = Error e
  fmap f (Result a) = Result (f a)

instance Monad (MayFail e) where
  return = Result
  Error e >>= f = Error e
  Result a >>= f = f a

instance Applicative (MayFail e) where
  pure = return
  (<*>) = ap

eval :: Exp -> MayFail String Int
eval (Lit a) = return a
eval (Add a b) = eval a >>= \av ->
                 eval b >>= \bv ->
                 return (av + bv)
eval (Mul a b) = eval a >>= \av ->
                 eval b >>= \bv ->
                 return (av * bv)
eval (Div a b) = eval a >>= \av ->
                 eval b >>= \bv ->
                 safeDiv av bv

eval' :: Exp -> MayFail String Int
eval' (Lit a) = return a
eval' (Add a b) = do
  av <- eval a
  bv <- eval b
  return (av + bv)
eval' (Mul a b) = do
  av <- eval a
  bv <- eval b
  return (av * bv)
eval' (Div a b) = do
  av <- eval a
  bv <- eval b
  safeDiv av bv
