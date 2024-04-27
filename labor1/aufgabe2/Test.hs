{-# LANGUAGE TemplateHaskell #-}
module Test where

import Printf



result = $(printf "Mein Name ist %s und ich bin %d Jahre alt") "Elias" 22