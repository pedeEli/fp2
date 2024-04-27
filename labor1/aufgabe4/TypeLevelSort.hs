{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module TypeLevelSort where


import FunFunDeps

data Nil
data Cons a as


class DownFrom n xs | n -> xs where
  downfrom :: n -> xs
  downfrom = u

instance DownFrom Zero Nil
instance DownFrom n ns => DownFrom (Succ n) (Cons n ns)


class Lte a1 a2 b | a1 a2 -> b where
  lte :: a1 -> a2 -> b
  lte = u

instance Lte Zero n True
instance Lte (Succ n) Zero False
instance Lte n1 n2 b => Lte (Succ n1) (Succ n2) b


class Insert x xs ys | x xs -> ys where
  insert :: x -> xs -> ys
  insert = u
class InsertCons b x1 x2 xs ys | b x1 x2 xs -> ys where
  insertcons :: b -> x1 -> x2 -> xs -> ys
  insertcons = u

instance Insert n Nil (Cons n Nil)
instance (Lte x y b, InsertCons b x y ys zs) => Insert x (Cons y ys) zs

instance InsertCons True x1 x2 xs (Cons x1 (Cons x2 xs))
instance Insert x1 xs ys => InsertCons False x1 x2 xs (Cons x2 ys)


class Sort xs ys | xs -> ys where
  sort :: xs -> ys
  sort = u

instance Sort Nil Nil
instance (Insert x ys zs, Sort xs ys) => Sort (Cons x xs) zs