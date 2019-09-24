import Control.Monad.RWS

{-# LANGUAGE FlexibleContexts #-}

type R = [Int]
-- ask
-- asks

type W = [String]
-- tell

type S = [Int]
-- get
-- put

type Comp a = RWS R W S a

pop :: Comp ()
pop = do
    state <- get
    case state of
      [] -> tell ["Did not pop"]
      (x:xs) -> tell ["pop " ++ show x] >> put xs

push :: Int -> Comp ()
push x = do
    state <- get
    tell ["push " ++ show x]
    put (x:state)


computation :: Int -> Comp ()
computation = do
    push 3
    pop

main :: IO ()
main = print $ runRWS computation [] [1, 2]

