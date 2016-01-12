-- Session 2 - 1 Datatype Drill

import Data.List
-- 1.1
data Tree a = EmptyTree
            | TreeNode a (Tree a) (Tree a)
            deriving Show

--data ChessPiece = ChessPiece String (Pos -> [Pos])

data Station a b = Machine [(a,Int)]  b
                 | Combine [Station a b]

-- 1.2

data IntTree = EmptyIntTree
             | IntTreeNode Int IntTree IntTree
             deriving Show

instance Eq IntTree where
  EmptyIntTree == EmptyIntTree = True
  IntTreeNode i l r == IntTreeNode j m s = (i == j) && (l == m) && (r == s)
  _ == _ = False

--OR IntTreeNOde i l r == IntTreeNode j m s
--                     | i == j
--                     , l == m
--                     , r == s = True

mapIntTree :: (Int -> Int) -> IntTree -> IntTree
mapIntTree _ EmptyIntTree = EmptyIntTree
mapIntTree f (IntTreeNode i l r) = IntTreeNode (f i) (mapIntTree f l) (mapIntTree f r)

intTree2list :: IntTree -> [Int]
intTree2list EmptyIntTree = []
intTree2list (IntTreeNode i l r) = i : intTree2list l ++ intTree2list r

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ EmptyTree = EmptyTree
mapTree f (TreeNode v l r) = TreeNode (f v) (mapTree f l) (mapTree f r)

instance Functor Tree where
  fmap = mapTree

tree2list :: Tree a -> [a]
tree2list EmptyTree = []
tree2list (TreeNode v l r) = v : tree2list l ++ tree2list r

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f x = foldr f x . tree2list

instance Eq a => Eq (Tree a) where
  t1 == t2 = sameElems (tree2list t1) (tree2list t2)

sameElems :: Eq a => [a] -> [a] -> Bool
sameElems [] ys = null ys
sameElems _ [] = False
sameElems (x:xs) ys
          | elem x ys = sameElems xs (delete x ys)
          | otherwise = False
