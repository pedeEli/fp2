{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Test where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.Type.Ord
import Data.Type.Equality
import Unsafe.Coerce


type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil  :: Vec 0 a
  (:>) :: KnownNat n => a -> Vec n a -> Vec (n + 1) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)



addingEquivalents :: forall n m t proxy. (KnownNat n, KnownNat m, KnownNat t, (n + t) ~ (m + t)) => proxy t -> n :~: m
addingEquivalents _ = case sameNat @n @m Proxy Proxy of
  Just Refl -> Refl
  Nothing -> error "Contradiction"


assoc :: forall n m p proxy. (KnownNat n, KnownNat m, KnownNat p) => proxy n -> proxy m -> ((n + m) + p) :~: (n + (p + m))
assoc _ _ = unsafeCoerce Refl



head :: Vec (n + 1) a -> a
head (a :> _) = a

tail :: forall n a. KnownNat n => Vec (n + 1) a -> Vec n a
tail (_ :> (as :: Vec m a)) = case addingEquivalents @n @m @1 Proxy of Refl -> as



reverse :: KnownNat n => Vec n a -> Vec n a
reverse = go Nil
  where
    go :: forall m p a. (KnownNat m, KnownNat p) => Vec m a -> Vec p a -> Vec (m + p) a
    go acc Nil       = acc
    go acc (a :> (as :: Vec n1 a)) = case assoc @m @1 @n1 Proxy Proxy of
      Refl -> go (a :> acc) as


vec = 1 :> 2 :> Nil

-- t = tail vec