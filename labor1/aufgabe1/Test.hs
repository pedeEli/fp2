{-# LANGUAGE TemplateHaskell #-}
module Test where

import CurryN


test = $(curryN 3)