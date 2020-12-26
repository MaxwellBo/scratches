-- Nil = []
-- Cons = (:)

-- type Optional a = Maybe a
-- Just = Full
-- Empty = Nothing 


liftIntoMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftIntoMaybe f (Just a) (Just b) = Just (f a b)
liftIntoMaybe _ _ _ = Nothing


sequenceListMaybe :: [Maybe a] -> Maybe [a]
sequenceListMaybe [] = Just []
sequenceListMaybe (x:xs) = let 
    liftedCons = liftIntoMaybe (:)
  in 
    liftedCons x (sequenceListMaybe xs)


traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe f xss = sequence (fmap f xss)

f = (\x -> Just (x + 1))
xss = [1, 2, 3]

main = print (traverseListMaybe f xss)
checkWin :: GameState -> Maybe Winner 
checkWin (GameState []) = Nothing
checkWin (GameState game@(x:xs)) =
  let
    decideVictor :: Solution -> Winner
    decideVictor solution =
      if odd . length $ game
        then P1Win solution
        else P2Win solution
  in
    decideVictor <$> (checkSum15 . grabPlayerMoves $ game)


  
    [] -> Nothing -- can never be reached
    (x:xs) -> case (odd (length (x:xs)), checkSum15 (grabPlayerMoves game)) of -- True == Player 1, False == Player 2
        (True, Just a) ->  Just (P1Win a)
        (False, Just b) -> Just (P2Win b) 
        (_, Nothing) -> Nothing