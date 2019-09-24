module Main

import Data.Fin
import Data.Vect

main : IO () 
main = putStrLn "Hello world!"

go : Fin 3
go = FZ
-- go = FS (FS (FS (FZ)))

data IsElem : a -> Vect n a -> Type where
   Here :  {x:a} ->   {xs:Vect n a} -> IsElem x (x :: xs)
   There : {x,y:a} -> {xs:Vect n a} -> IsElem x xs -> IsElem x (y :: xs)
   
testVec : Vect 1 Int
testVec = 3 :: Nil

inVect : IsElem 3 Main.testVec
inVect = Here
