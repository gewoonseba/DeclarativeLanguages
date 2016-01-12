--Session 2 - 2 Sequences
import Data.Char

class Sequence a where
  next :: a -> a
  prev :: a -> a

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance Sequence Int where
  next x = x + 1
  prev x = x - 1

instance LeftBoundedSequence Int where
  firstElem = minBound

instance RightBoundedSequence Int where
  lastElem = maxBound

instance Sequence Char where
  next 'z' = "no char after z"
  next c = chr $ succ $ ord
  prev 'a' = "no char before a"
  prev c = chr $ pred $ ord

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance RightBoundedSequence Char where
  lastElem = 'z'

instance Sequence Bool where
  next = not
  prev = not

instance LeftBoundedSequence Bool where
  firstElem = False

instance RightBoundedSequence Boool where
  lastElem = True
  
