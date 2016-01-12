-- Sebastian Stoelen
-- r0381219
-- master cw

{- | Symbolic differentiation exam question

     The purpose of this exercise is to implement symbolic differentation for
     a simplistic expression language consisting of constants, variables,
     addition, multiplication, exponentiation and natural logarithms.

     Author: Alexander Vandenbroucke
-}

module SymbolicDifferentiation where

-- | Function type, represents functions f : R -> R (where R is the real
--   numbers), build from multiplication, addition, exponentiation with a
--   real constant and the natural logarithm.
data Function
  = Const Double
    -- ^ Constant
  | X
    -- ^ variable
  | Function :+: Function
    -- ^ addition
  | Function :*: Function
    -- ^ multiplication
  | Function :^: Double
    -- ^ integer power
  | Ln Function
    -- ^ natural logarithm
  deriving (Show, Eq)

infixl 6 :+:
infixl 7 :*:
infixl 8 :^:

-------------------------------------------------------------------------------
-- Exercise 1: Complete the definition of the 'Num' instance below. Fill in
-- the undefineds for +, * and negate with the appropriate definition.
-- If you cannot complete this exercise, be careful when typing in the
-- examples: using a (+) or (*) instead of (:+:) or (:*:) will cause an error!

instance Num Function where
  fromInteger = Const . fromInteger
  f + g = f :+: g
  f * g = f :*: g
  abs = error "Function.Num.abs: undefined"
  signum = error "Function.Num.signum: undefined"
  negate f = undefined

-------------------------------------------------------------------------------
-- Exercise 2: Function Evaluation

-- | Evaluate a Function.
--   'evaluate f a' evaluates 'f' by substituting every 'X' with 'a', and
--   doing the obvious thing for 'Const', ':+:', ':*:', ':^:' and 'Ln'.
--   Some of these functions are not defined for every Double, use the
--   Maybe-monad to return Nothing when the function is undefined.

evaluate :: Function -> Double -> Maybe Double
evaluate (Const a) _ = return a
evaluate X d = return d
evaluate (a :+: b) d = do
      av <- evaluate a d
      bv <- evaluate b d
      return (av + bv)
evaluate (a :*: b) d = do
      av <- evaluate a d
      bv <- evaluate b d
      return (av * bv)
evaluate ( a :^: b) d = do
      av <- evaluate a d
      evaluatePw av b
evaluate (Ln a) d = do
      av <- evaluate a d
      evaluateLn av

evaluateLn :: Double -> Maybe Double
evaluateLn a
      | a <= 0 = Nothing
      | otherwise = return(log a)

evaluatePw :: Double -> Double -> Maybe Double
evaluatePw a b
      | a == 0, b <= 0 = Nothing
      | otherwise = return(a ** b)

-------------------------------------------------------------------------------
-- Exercise 3: Derivative

-- | Compute the derivative of a function by interpreting its structure,
--   and applying the rules for computing derivatives.
derivative :: Function -> Function
derivative (Const _) = Const 0.0
derivative X = Const 1.0
derivative (a :+: b) = (derivative a) :+: (derivative b)
derivative (a :*: b) = ((derivative a) :*: b) :+: (a :*: (derivative b))
derivative (a :^: b) = (Const b :*: a :^: (b - 1)) :*: (derivative a)
derivative (Ln a) = (a :^: (-1)) :*: (derivative a)

-------------------------------------------------------------------------------
-- Exercise 4: Pretty printing

pretty :: Function -> String
pretty f = pretty' 0 f where

-- | "pretty' c f" takes an integer c that is the precedence
--   of the operator of which f occurs as an argument position (i.e. the
--   context of f).
--   This means that a function should only be enclosed in parentheses if
--   it occurs in a context that has higher priority than itself.
--   =======================
--    operator    priority
--   -----------------------
--    Ln             4
--    Const, X       3
--    :^:            2
--    :*:            1
--    :+:            0
--   =======================
pretty' :: Int -> Function -> String
pretty' _ (Ln a) = "ln" ++ pretty' 4 a
pretty' con (Const a)
      | con > 3 = "(" ++ (show a) ++ ")"
      | otherwise = show a
pretty' con X
      | con > 3 = "(X)"
      | otherwise = "X"
pretty' con (a :^: b)
      | con > 2 = "(" ++ (pretty' 2 a) ++ "^" ++ (show b) ++ ")"
      | otherwise = (pretty' 2 a) ++ "^" ++ (show b)
pretty' con (a :*: b)
      | con > 1 = "(" ++ (pretty' 1 a) ++ " * " ++ (pretty' 1 b) ++ ")"
      | otherwise = (pretty' 1 a) ++ " * " ++ (pretty' 1 b)
pretty' con (a :+: b)
      | con > 0 = "(" ++ (pretty' 1 a) ++ " + " ++ (pretty' 1 b) ++ ")"
      | otherwise = (pretty' 1 a) ++ " + " ++ (pretty' 1 b)

-------------------------------------------------------------------------------
-- Exercise 5: User Interface

-- | Mini command-line application to pretty-print, evaluate and take the
--   derivative.
--   If you did not complete 'pretty', use 'show' instead
--   If you did not complete 'derivative', re-evaluate the Function instead.
evaluateIO :: Function -> IO ()
evaluateIO f = do
  putStrLn (pretty f)
  inp <- getLine
  putStrLn . show (evaluate f (read inp))
  putStrLn (pretty (derivative f))
  inp2 <- getLine
  putStrLn . show (evaluate (derivative f) (read inp2))
  return ()
