import Data.Function
import Control.Category ((>>>))

newtype DList a =  DL { unDL :: [a] -> [a] } 

empty = DL id

toList :: DList a -> [a]
toList  = ($[]) . unDL

fromList :: [a] -> DList a
fromList = DL . const

cons :: a -> DList a -> DList a
cons x = DL . ((x:).) . unDL

snoc :: DList a -> a -> DList a
snoc xs x = DL . ((x:)>>>) . unDL $ xs 

(+++) :: DList a -> DList a -> DList a
xs +++ ys = DL (unDL xs . unDL ys)
