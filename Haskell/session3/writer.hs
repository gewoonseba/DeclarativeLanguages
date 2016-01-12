import Control.Applicative (Applicative(..))
import Control.Monad (liftM2, join, ap)

--Session 3 -4 Writer Monad

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
        deriving (Show)

data Writer a = Writer a String
        deriving (Show)

evalTrace :: Exp -> (Int, String)
evalTrace (Lit a) = (a, "Lit\n")
evalTrace (Add a b) = (fst(evalTrace a) + fst(evalTrace b), "Add\n" ++ snd(evalTrace a) ++ "Add\n" ++ snd(evalTrace b))
evalTrace (Mul a b) = (fst(evalTrace a) * fst(evalTrace b), "Mul\n" ++ snd(evalTrace a) ++ "Mul\n" ++ snd(evalTrace b))

instance Functor Writer where
  fmap f (Writer a log) = Writer (f a) log

instance Applicative Writer where
  pure = return
  (<*>) = ap

instance Monad Writer where
  return x = Writer x ""
  Writer a log >>= f =
    let Writer x log' = f a
    in Writer x (log ++ log')

trace :: String -> Writer ()
trace s = Writer () s

evalTraceM :: Exp -> Writer Int
evalTraceM (Lit x) = do
  trace "Lit\n"
  return x
evalTraceM (Add x y) = do
  trace "Add\n"
  xv <- evalTraceM x
  yv <- evalTraceM y
  return (xv + yv)
evalTraceM (Mul x y) = do
  trace "Mul\n"
  xv <- evalTraceM x
  yv <- evalTraceM y
  return (xv * yv)

--equivalent zonder do 
evalTraceM2 :: Exp -> Writer Int
evalTraceM2 (Lit x) = trace "Lit\n" >>= \f ->
                     return x
evalTraceM2 (Add x y) = trace "Add\n" >>= \f ->
                        evalTraceM2 x >>= \xv ->
                        evalTraceM2 y >>= \yv ->
                        return (xv + yv)
evalTraceM2 (Mul x y) = trace "Mul\n" >>= \f ->
                        evalTraceM2 x >>= \xv ->
                        evalTraceM2 y >>= \yv ->
                        return (xv * yv)
