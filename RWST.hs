import Control.Monad.RWS

type R = [Int]
-- ask
-- asks

type W = [String]
-- tell

type S = [Int]
-- get
-- put

pop :: RWST R W S Maybe Int
pop = do
    state <- get
    case state of
      [] -> tell ["Did not pop"] >> lift Nothing
      (x:xs) -> tell ["pop " <> show x] >> put xs >> return x

push :: Int -> (RWST R W S Maybe Int)
push x = do
    state <- get
    tell ["push " ++ show x]
    put (x:state)
    return x

computation :: RWST R W S Maybe Int
computation = do
    push 3
    pop
    x <- pop
    tell ["popped " <> show x]
    return x

main :: IO ()
main = print $ runRWST computation [] [1, 2]

