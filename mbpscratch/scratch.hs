mapPlusOne :: (Functor f) => f Int -> f Int 
mapPlusOne = fmap (+1)

res :: Either String [Int]
res = do
  x <- Right [5]
  y <- Left "COULD NOT RECOVER AN INTEGER"
  return $ x ++ y


readThenPrint :: IO ()
readThenPrint = readLn >>= putStrLn
