{-# LANGUAGE TemplateHaskell #-}
module Test where

import CurryN


foo :: (Int, Int, Int) -> Int
foo (a, b, c) = a + b + c

$(genCurries 10)