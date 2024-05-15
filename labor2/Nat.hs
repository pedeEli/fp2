{-# LANGUAGE DataKinds, TypeFamilies #-}
module Nat where


import Data.Kind

data Nat = Z | S Nat

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

instance Show Nat where
  show = show . natToInt


type One = S Z
type Two = S One
type Four = S (S Two)


(#+) :: Nat -> Nat -> Nat
Z #+ n = n
n #+ Z = n
(S n1) #+ n2 = n1 #+ S n2

(#*) :: Nat -> Nat -> Nat
Z #* n = Z
n #* Z = Z
(S n1) #* n2 = n2 #+ (n1 #* n2)

(#^) :: Nat -> Nat -> Nat
Z #^ Z = error "undefined"
Z #^ n = Z
n #^ Z = S Z
n1 #^ (S n2) = n1 #* (n1 #^ n2)


type SNat :: Nat -> Type
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)


type family (m :: Nat) :< (n :: Nat) :: Bool
type instance m     :< Z     = False
type instance Z     :< (S n) = True
type instance (S m) :< (S n) = m :< n