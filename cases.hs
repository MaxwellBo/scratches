solve :: Double -> Double
solve x = sum . fmap ($x). fmap (term) $ [0..10]

factorial :: Double -> Double
factorial n = product [1..n]

term :: Int -> (Double -> Double)
term 0 = (\_ -> 1)
term 1 = id
term y = (\x -> x^y / factorial x)

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words

-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

main :: IO ()
main = do
    cases <- getLine
    replicateM_ (read cases) testCases 
    
testCases :: IO ()
testCases = do
    pairsCount <- getLine
    pairs <- (replicateM (read pairsCount) getLine)
    mapM_ putStrLn pairs
    return ()
    
-- checkPairs :: [String] -> Bool
-- checkPairs pairs =