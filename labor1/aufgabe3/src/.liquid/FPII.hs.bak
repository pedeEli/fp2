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

-- implement insertion sort with explicit recursion
insertionSort :: Ord a => [a] -> SortedList a
insertionSort = undefined

-- use a suitable higher order function for the same algorithm
insertionSort' :: Ord a => [a] -> SortedList a
insertionSort' = undefined

quickSort :: Ord a => [a] -> SortedList a
quickSort = undefined

mergeSort :: Ord a => [a] -> SortedList a
mergeSort = undefined