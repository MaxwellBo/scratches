import Control.Monad.State

type ReturnType = (Int, Int)
type StateType = Int

type Our = State StateType ReturnType

main :: IO ()
main = print $ runState computation initialState 
  where initialState = 0

computation :: Our
computation = do
  old <- get
  modify (+1)
  new <- get
  return (old, new)