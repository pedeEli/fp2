{-# LANGUAGE GADTs, DataKinds #-}
module Vec where


import Nat
import Data.Kind

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Z a
  (:-) :: a -> Vec n a -> Vec (S n) a
infixr 5 :-


instance Show a => Show (Vec n a) where
  show Nil = "[]"
  show as = "[" ++ go as ++ "]"
    where
      go :: Show a => Vec n1 a -> String
      go (a :- Nil) = show a
      go (a :- as) = show a ++ ", " ++ go as


instance Functor (Vec n) where
  fmap _ Nil = Nil
  fmap f (a :- as) = f a :- fmap f as

instance Foldable (Vec n) where
  foldr :: (a -> b -> b) -> b -> Vec n a -> b
  foldr _ init Nil = init
  foldr f init (a :- as) = f a $ foldr f init as


(@+) :: Num a => Vec n a -> Vec n a -> Vec n a
Nil @+ Nil = Nil
(a :- as) @+ (b :- bs) = (a + b) :- (as @+ bs)


safeTail :: Vec (S n) a -> Vec n a
safeTail (_ :- as) = as



nth :: (m :< n) ~ True => SNat m -> Vec n a -> a
nth SZ     (a :- _)  = a
nth (SS m) (_ :- as) = nth m as



(#) :: Num a => Vec n a -> Vec n a -> a
Nil # Nil = 0
(a :- as) # (b :- bs) = (a * b) + (as # bs)