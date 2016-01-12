--Session 3 - 2 Functors

data Indentity a = Indentity a
                  deriving Show

instance Functor Indentity where
  fmap f (Indentity x) = Identity (f x)


data Pair a b = Pair a b
              deriving Show

instance Functor Pair where
  fmap f Pair a b = Pair a (f b)


data Unit a = Unit
            deriving Show

instance Functor Unit where
  fmap _ Unit = Unit 
