foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f c [] = c 
foldr' f c (x:xs) = x `f` (foldr' f c xs)



map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr' ((:) . f) [] xs
