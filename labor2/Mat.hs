{-# LANGUAGE GADTs, DataKinds #-}
module Mat where



import Nat
import Vec
import Data.Kind



type Mat :: Nat -> Nat -> Type -> Type
data Mat rows cols a where
  Mat :: Num a => Vec rows (Vec cols a) -> Mat rows cols a

instance Show a => Show (Mat rows cols a) where
  show (Mat rows) = g $ fmap f rows
    where
      width = maximum $ fmap (maximum . fmap (length . show)) rows

      pad :: String -> String
      pad str = replicate (width - length str) ' ' ++ str

      f :: Vec n a -> String
      f Nil = ""
      f (a :- as) = pad (show a) ++ " " ++ f as

      g :: Vec n String -> String
      g Nil = "[]"
      g (a :- Nil) = a
      g (a :- as) = a ++ "\n" ++ g as


(!!!) :: (i :< rows ~ True, j :< cols ~ True) => Mat rows cols a -> (SNat i, SNat j) -> a
(Mat rows) !!! (i, j) = nth j $ nth i rows


(#@) :: Num a => Mat (S rows) (S cols) a -> Vec (S cols) a -> Vec (S rows) a
(Mat rows) #@ vec = fmap (vec #) rows


(#@@) :: Num a => Mat (S rows) (S n) a -> Mat (S n) (S cols) a -> Mat (S rows) (S cols) a
(#@@) = undefined 


mat = Mat $ (1 :- 22 :- 69 :- Nil) :- (4 :- 500 :- 42 :- Nil) :- Nil