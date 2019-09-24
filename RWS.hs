{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.RWS

type R = [Int]
-- ask
-- asks

type W = [String]
-- tell

type S = [Int]
-- get
-- put

type Comp a = RWS R W S a

pop :: (MonadState S m, MonadWriter W m) => (m ())
pop = do
  state <- get
  case state of
    [] -> tell ["Did not pop"]
    (x:xs) -> tell ["pop " ++ show x] >> put xs

push :: (MonadState S m, MonadWriter W m) => Int -> (m ())
push x = do
  state <- get
  tell ["push " ++ show x]
  put (x:state)

computation :: Comp ()
computation = do
  push 3
  pop

main :: IO ()
main = print $ runRWS computation [] [1, 2]

