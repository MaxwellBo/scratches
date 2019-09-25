{-# LANGUAGE RankNTypes #-}
import Control.Monad (ap)

data List a = List (forall b. (a -> b -> b) -> b -> b)

unList :: List a -> [a]
unList = foldr (:) []

nil :: List a
nil = List $ const id

cons :: a -> List a -> List a
cons x (List xss) = List $ 
    \r b -> xss r (r x b)

instance Functor List where
  fmap f = foldr (cons . f) nil

instance Applicative List where
  pure a = cons a nil
  (<*>) = ap

instance Monad List where
  (>>=) fa f = concatL . (fmap f) $ fa

instance Foldable List where
  foldr r b (List xss) = xss r b

(+++) :: List a -> List a -> List a
(+++) xss yss = foldr cons yss xss

concatL :: List (List a) -> List a
concatL = foldr (+++) nil
