import Data.Semigroup
import Control.Applicative

data Validated a b = Invalid a 
                   | Valid b 
    deriving (Eq, Ord, Read, Show)

instance Functor (Validated e) where
  fmap _ (Invalid e) = Invalid e
  fmap f (Valid x) = Valid (f x)

instance (Semigroup e) => Applicative (Validated e) where
  pure = Valid
  (Invalid e) <*> (Invalid r) = Invalid (e <> r) 
  (Valid f) <*> (Valid x) = Valid (f x)
  (Invalid e) <*> _ = Invalid e
  _ <*> (Invalid e) = Invalid e
