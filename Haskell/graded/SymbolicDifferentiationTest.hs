module SymbolicDifferentiationTest where

import SymbolicDifferentiation
import Control.Monad

import Prelude hiding (catch) -- Required for older versions of GHC
import Data.Monoid
import Control.Exception (catch, SomeException(..))


main :: IO ()
main = startTests
       <> test "Ex1-Num-plus"   ((Const 0 + Const 1) == Const 0 :+: Const 1)
       <> test "Ex1-Num-times"  ((Const 0 * Const 1) == Const 0 :*: Const 1)
       <> test "Ex1-Num-negate" (negate X            == Const (-1) * X)
       <> test "Ex2-evaluate-1" (evaluate (1 :+: (2 :*: X):^:2) 1 == Just 5.0)
       <> test "Ex2-evaluate-2" (evaluate (Ln (2 :*: X:^:2 :+: (-4))) (1.0)
                                 == Nothing)
       <> test "Ex2-evaluate-3" (evaluate (X :*: X) 42 == evaluate (X:^:2) 42)
       <> test "Ex2-evaluate-4" (evaluate (Ln X) (exp 1) == Just (exp 0))
       <> test "Ex3-derivative-1" (evaluate (derivative (X:^:2 :*: X:^:3)) 4
                                   == Just 1280)
       <> (test "Ex3-derivative-2"
           (derivative (2 :*: Ln X)
            ==
            Const 0.0 :*: Ln X :+: Const 2.0 :*: (X :^: (-1.0) :*: Const 1.0)))
       <> test "Ex3-derivative-3" (derivative (X :+: X) == 1 :+: 1)
       <> test "Ex4-pretty-1" (pretty ((1 :+: 2) :*: 3) == "(1.0 + 2.0) * 3.0")
       <> test "Ex4-pretty-2" (pretty ((1 :*: 2) :*: 3) == "1.0 * 2.0 * 3.0")
       <> test "Ex4-pretty-3" (pretty (Ln (Ln X)) == "lnln(x)")
       <> test "Ex4-pretty-4" (pretty ((Ln (1 :+: X:^:2)):*:X)
                               ==
                               "ln(1.0 + x^2.0) * x")
       <> test "Ex4-pretty'-1" (pretty' 2 ((X :+: 1) :*: X)
                                == "((x + 1.0) * x)")
       <> test "Ex4-pretty'-2" (pretty' 0 (X :+: X) == "x + x")
       <> test "Ex4-pretty'-3" (pretty' 3 (Const 1) == "1.0")
       <> test "Ex4-pretty'-4" (pretty' 4 (Const 1) == "(1.0)")
       >>= endTests

-- Mini testing framework
test :: String -> Bool -> IO Results
test msg b
  = do notImplemented <- isUndefined b
       case notImplemented of
         True      -> printResult yellow "function not implemented" >> return (Sum 1, Sum 0, Sum 0)
         False | b -> printResult green "passed" >> return (Sum 0, Sum 0, Sum 1)
         _         -> printResult red "failed" >> return (Sum 0, Sum 1, Sum 0)
  where printResult colorCode result
          = putStrLn $ "Test " ++ msg ++ " " ++ colorise colorCode result

type Results = (Sum Int, Sum Int, Sum Int) -- (Not implemented, failed, passed)

instance Monoid a => Monoid (IO a) where
  mempty = return mempty
  mappend = liftM2 mappend

startTests :: IO Results
startTests = putStrLn "Testing your solutions" >> return (Sum 0, Sum 0, Sum 0)

endTests :: Results -> IO ()
endTests (notImpl, failed, passed)
  = case (getSum notImpl, getSum failed, getSum passed) of
     (0, 0, _) -> putStrLn $ colorise green "All tests passed"
     (n, f, p) -> putStrLn $ unwords $
                  filter (not . null) [nNotImpl n, nFailed f, nPassed p]
  where nPassed 0 = ""
        nPassed p = colorise green $ show p ++ " tests passed"
        nFailed 0 = ""
        nFailed f = colorise red $ show f ++ " tests failed"
        nNotImpl 0 = ""
        nNotImpl n = colorise yellow $ show n ++ "x function not implemented"

isUndefined :: a -> IO Bool
isUndefined a = (a `seq` return False) `catch` \(SomeException _) -> return True

red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"
