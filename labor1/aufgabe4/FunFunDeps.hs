{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module FunFunDeps where

import Prelude hiding (and, or, even, odd, pred)

u = undefined

data True
data False

class And b1 b2 b | b1 b2 -> b where and :: b1 -> b2 -> b
class Or b1 b2 b | b1 b2 -> b where or :: b1 -> b2 -> b
class Not b1 b where not :: b1 -> b

instance And True  True  True  where and = u
instance And True  False False where and = u
instance And False True  False where and = u
instance And False False False where and = u

instance Or True  True  True  where or = u
instance Or True  False True  where or = u 
instance Or False True  True  where or = u
instance Or False False False where or = u

instance Not True  False where not = u
instance Not False True  where not = u

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four

class Even n b | n -> b where even :: n -> b
class Odd n b | n -> b where odd :: n -> b

instance Even Zero True  where even = u
instance Odd  Zero False where odd  = u

instance (Even n b) => Odd  (Succ n) b where odd  = u
instance (Odd  n b) => Even (Succ n) b where even = u

class Add n1 n2 n | n1 n2 -> n where add :: n1 -> n2 -> n
class Mul n1 n2 n | n1 n2 -> n where mul :: n1 -> n2 -> n

instance                Add Zero     n n        where add = u
instance (Add a b c) => Add (Succ a) b (Succ c) where add = u

instance                           Mul Zero     n Zero where mul = u
instance (Mul a b c, Add b c d) => Mul (Succ a) b d    where mul = u 

-- mixing static and dynamic calc

-- dynamic
pow' b 0 = 1
pow' b n = b * pow' b (n-1)

-- static
class Pow a b c | a b -> c where pow'' :: a -> b -> c

instance                           Pow a Zero     One where pow'' = u
instance (Pow a b d, Mul a d c) => Pow a (Succ b) c   where pow'' = u

-- mixed
class Pred a b | a -> b  where pred :: a -> b
instance Pred (Succ n) n where pred = u

class Power n where power :: Int -> n -> Int
instance            Power Zero     where power _ _ = 1
instance Power n => Power (Succ n) where
    power x n = x * power x (pred n)

res = power 2 (mul (u :: Four) (u :: Two))
