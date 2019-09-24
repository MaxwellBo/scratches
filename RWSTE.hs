import Control.Monad.RWS

type R = [Int]
-- ask
-- asks

type W = [String]
-- tell

type S = [Int]
-- get
-- put

pop :: RWST R W S (Either String) (Maybe Int)
pop = do
    state <- get
    case state of
      [] -> lift $ Left "Popped an empty stack"
      (x:xs) -> tell ["pop " <> show x] >> put xs >> return (Just x)

push :: Int -> RWST R W S (Either String) (Maybe Int)
push x = do
    state <- get
    tell ["push " <> show x]
    put (x:state)
    return Nothing

computation :: RWST R W S (Either String) (Maybe Int)
computation = do
    push 3
    Just x <- pop
    Just y <- pop
    tell ["Popped " <> show x <> " and " <> show y]
    return Nothing

main :: IO ()
main = do
    case runRWST computation [] [1, 2] of
      result@(Right (_, _, writer)) -> (print result) >> (putStrLn $ unlines $ writer)
      Left err -> putStrLn err

