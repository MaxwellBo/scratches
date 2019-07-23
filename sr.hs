
import Data.Semigroup

multiply :: [Int]
multiply = do
    x <- [ 1, 2, 3 ]
    y <- [ 2, 3, 4 ]
    return $ x * y

tuple :: [(Int, Int)]
tuple = do
  x <- [1,2,3]
  y <- [2,5,4]
  return (x, y)

tuple' = [1,2,3] >>= (\x -> [2,3,4] >>= (\y -> return (x,y)))

straightMonad = do
  x <- Just 5
  y <- Just 6
  return (x, y)

straightApplicative = (,) <$> Just 5 <*> Just 6

decision = do
  x <- Just 5
  y <- if x == 5 then Nothing else Just 6
  return (x, y)

decision' = Just 5 >>= (\x -> if x == 5 then Nothing else Just 6 >>= (\y -> return (x,y)))


--------------------------------------------------------------------------------

validateUsername :: String -> Either String String
validateUsername _ = Left "bad username"

validatePassword :: String -> Either String String
validatePassword _ = Left "bad password"

validateCreds :: String -> String -> Either String (String, String)
validateCreds username password = do
  username <- validateUsername username
  password <- validatePassword password
  return (username, password)

data Validated e a = Invalid e | Valid a deriving (Show)

instance Functor (Validated e) where
  fmap f (Valid x) = Valid (f x)
  fmap _ (Invalid e) = Invalid e

instance Semigroup e => Applicative (Validated e) where
  Valid f <*> Valid a = Valid (f a)
  Invalid e <*> Valid a = Invalid e
  Valid f <*> Invalid e = Invalid e
  Invalid e <*> Invalid g = Invalid (e <> g)
  pure = Valid

-- instance Monad (Validated e) where
--   Valid a >>= f = f a
--   Invalid e >>= f = ???
-- _it is impossible to write a Monad instance for Validated_

validateUsername' :: String -> Validated [String] String
validateUsername' _ = Invalid ["bad username"]

validatePassword' :: String -> Validated [String] String
validatePassword' _ = Invalid ["bad password"]

validateCreds' :: String -> String -> Validated [String] (String, String)
validateCreds' username password = (,) <$> validateUsername' username <*> validatePassword' password


--------------------------------------------------------------------------------

-- instance Functor ((->) r) where
--     fmap = (.)
--
-- instance Applicative ((->) a) where
--     pure = const
--     (<*>) f g x = f x (g x)
--     liftA2 q f g x = q (f x) (g x)
--
-- instance Monad ((->) r) where
--     f >>= k = \ r -> k (f r) r

type Context = Int

readFromContext :: Context -> String
readFromContext = do
  x <- (+1)
  y <- (+3)
  return $ show (x * y)


--------------------------------------------------------------------------------

newtype State s a = State { runState :: s -> (a,s) }

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap _ Nothing  = Nothing

instance Functor (State s) where
  fmap f (State h) = State $ \s -> let (a, newState) = (h s) 
                                       b = f a
                                   in (b, newState)

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--   pure                  = Just
--   (Just f) <*> (Just x) = Just (f x)
--   _        <*> _        = Nothing

instance Applicative (State s) where
  pure x = State $ \s -> (x,s)  
  (State mf) <*> (State ma) = State $ \s -> let (f, newState) = mf $ s
                                                (a, reallyNewState) = ma $ newState
                                                b = f a
                                            in (b, reallyNewState)

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

--   return      :: a -> m a
--   return      = pure

-- instance  Monad Maybe  where
--   (Just x) >>= k      = k x
--   Nothing  >>= _      = Nothing

instance Monad (State s) where  
  return x = State $ \s -> (x,s)  
  (State ma) >>= f = State $ \s ->  let (a, newState) = ma s  
                                        (State mb) = f a  
                                    in mb newState  

push :: Int -> State [Int] ()
push x = State $ \s -> ((), x:s)

pop :: State [Int] Int
pop = State $ \(x:xs) -> (x, xs)

stack :: State [Int] Int
stack = do
  push 3
  push 4
  x <- pop
  return x

--------------------------------------------------------------------------------

increment :: Int -> (Sum Int, ())
increment x = (Sum x, ())

state :: (Sum Int, ())
state = do
  increment 4
  x <- return 5
  traverse increment [1..x]
  increment 3
