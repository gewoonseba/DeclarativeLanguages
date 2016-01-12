-- Sebastian Stoelen
-- r0381219
-- master cw

module MOPL where
--Exercise 1
data Statement = Assignment String Term| Print Term
data Term = Variable String | Integer Int | Function (Int -> Int -> Int) Term Term

--Exercise 2
assign :: String -> Term -> Statement
assign = Assignment

printTerm :: Term -> Statement
printTerm = Print

intTerm :: Int -> Term
intTerm = Integer

varTerm :: String -> Term
varTerm = Variable

plus :: Term -> Term -> Term
plus = Function (+)

times :: Term -> Term -> Term
times = Function (*)

minus :: Term -> Term -> Term
minus = Function (-)

--Exercise 3
type State = [(String,Int)]

valueOf :: State -> String -> Int
valueOf ss k = head [snd(s) | s <- ss, fst(s) == k]

insertS :: String -> Int -> State -> State
insertS k i ss= (k, i) : [ s | s <- ss, fst(s) /= k]

--Exercise 4
evalTerm :: State -> Term -> Int
evalTerm _ (Integer a) = a
evalTerm ss (Variable a) = valueOf ss a
evalTerm ss (Function f a b) = f av bv
    where av = evalTerm ss a
          bv = evalTerm ss b

--Exercise 5
execAssign :: String -> Term -> State -> State
execAssign k t st = insertS k (evalTerm st t) st

--Exercise 6
type Program = [Statement]

execPure :: State -> Program -> State
execPure = foldl go
    where go st (Print _) = st
          go st (Assignment a b) = execAssign a b st

--Exercise 7
execute :: Program -> IO ()
execute = executeHelper []

executeHelper :: State -> Program -> IO ()
executeHelper _ [] = return ()
executeHelper state (s : prog) = do
  nstate <- executeStatement state s
  executeHelper nstate prog

executeStatement :: State -> Statement -> IO State
executeStatement st (Print a) = do
  print (evalTerm st a)
  return st
executeStatement st (Assignment a b) = return (execAssign a b st)
