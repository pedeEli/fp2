{-# LANGUAGE ScopedTypeVariables #-}
{-@ LIQUID "--no-termination" @-}

module FPII where

{-@ data SortedList a = Emp | Cons { hd :: a, tl :: SortedList {v:a | v > hd}} @-}
data SortedList a =
  Emp | Cons { hd :: a, tl :: SortedList a }
  deriving (Eq, Show)

testListSuc :: SortedList Int
testListSuc = Cons 1 (Cons 2 (Cons 3 Emp))

testListFail :: SortedList Int
testListFail = Cons 1 (Cons 3 (Cons 10 Emp))

insert :: (Ord a) => a -> SortedList a -> SortedList a
insert x Emp = Cons x Emp
insert x (Cons y ys)
    | x < y    = Cons x (Cons y ys)
    | x == y   = insert y ys
    | otherwise = Cons y (insert x ys)

-- implement insertion sort with explicit recursion
insertionSort :: (Ord a) => [a] -> SortedList a
insertionSort [] = Emp
insertionSort (x:xs) = insert x (insertionSort xs)

-- use a suitable higher order function for the same algorithm
insertionSort' :: Ord a => [a] -> SortedList a
insertionSort' = foldr insert Emp


quickSort :: (Ord a) => [a] -> SortedList a
quickSort [] = Emp
quickSort (x:xs) = append x lessers greaters
  where
    lessers  = quickSort [y | y <- xs, y < x ]
    greaters = quickSort [z | z <- xs, z > x]


{-@ append :: x:a -> SortedList {lesser:a | lesser < x} -> SortedList {greater:a | greater > x} -> SortedList a @-}
append :: a -> SortedList a -> SortedList a -> SortedList a
append z Emp ys = Cons z ys
append z (Cons x xs) ys = Cons x $ append z xs ys




merge :: (Ord a) => SortedList a -> SortedList a -> SortedList a
merge Emp Emp = Emp
merge (Cons x xs)  Emp = Cons x xs
merge Emp (Cons y ys)  = Cons y ys
merge (Cons x xs) (Cons y ys)
  | x < y     = Cons x $ merge xs (Cons y ys)
  | x > y     = Cons y $ merge (Cons x xs) ys
  | otherwise = Cons x $ merge xs ys


split          :: [a] -> ([a], [a])
split (x:y:zs) = (x:xs, y:ys)
  where (xs, ys)   = split zs
split xs       = (xs, [])

mergeSort :: Ord a => [a] -> SortedList a
mergeSort [] = Emp
mergeSort [x] = Cons x Emp
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
  where (ys, zs) = split xs