import Control.Arrow

x :: Kleisli Maybe Int Int
x = Kleisli $ \x -> Just (x + 1)

y :: Kleisli Maybe Int Int
y = Kleisli $ \x -> Just (x + 2)

-- comp = do
--   a <- x
--   b <- y
--   return (a, b)

somefunc :: (Bifunctor t, Foldable t, Applicative f) => b (t a) (t a)
somefunc = undefined

comp' :: Int -> (Int, Int)
comp' = do
  x <- (+1)
  y <- (+2)
  return (x, y)

main :: IO ()
main = do
  -- print ((runKleisli comp) =<< return 5)
  print (comp' $ 5)