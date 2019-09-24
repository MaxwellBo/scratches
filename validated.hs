import Data.Semigroup
import Control.Applicative
import Data.Foldable

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

instance (Semigroup e) => Monad (Validated e) where
  (Valid x) >>= f = f x
  (Invalid x) >>= _  = Invalid x

--

type Args = ([ResourceId], Instant, Instant)

type ResourceId = Int
type Instant = Int

validDuration :: Args -> Validated [String] ()
validDuration (_, s, e) = if (s - e) > 24 then Invalid ["Greater than 24 hours"] else Valid ()

validResourceIds :: Args -> Validated [String] ()
validResourceIds (r, _, _) = if (length r > 1) then Invalid ["ID List too long"] else Valid ()

validGroup :: Args -> Validated [String] ()
validGroup args = mapM_ ($ args) [validDuration, validResourceIds]

check1 :: Args -> Validated [String] ()
check1 _ = Valid ()

check2 :: Args -> Validated [String] ()
check2 _ = Invalid ["Failure"]

checksGroup :: Args -> Validated [String] ()
checksGroup args = mapM_ ($ args) [check1, check1]

{-
HERE BE DRAGONS
We're exploiting the fact that the Validated instance is incoherant ->
as in, the applicative instance and the monad instance have different effects.

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
-}

validate :: Args -> Validated [String] Args
validate args = (traverse_ ($ args) [validGroup, checksGroup]) *> pure args
-- Present validated args to the next computation if all checks suceed

computation :: Validated [String] Int
computation = params >>= validate >>= (\(_, s, e) -> pure (s + e))

params :: Validated [String] Args
params = (,,) <$> Valid [1] <*> Valid 2 <*> Valid 3
