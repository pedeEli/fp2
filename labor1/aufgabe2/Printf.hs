{-# LANGUAGE TemplateHaskellQuotes #-}
module Printf where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Format = D | S | L String
  deriving Show


parse :: String -> [Format]
parse str = f str Nothing
  where
    f :: String -> Maybe String -> [Format]
    f [] Nothing = []
    f [] (Just l) = [L l]
    f ('%':'s':rest) Nothing = S : f rest Nothing
    f ('%':'s':rest) (Just l) = L l : S : f rest Nothing
    f ('%':'d':rest) Nothing = D : f rest Nothing
    f ('%':'d':rest) (Just l) = L l : D : f rest Nothing
    f (s:rest) Nothing = f rest (Just [s])
    f (s:rest) (Just l) = f rest (Just $ l ++ [s])


gen :: [Format] -> Q Exp -> Q Exp
gen [] acc = acc
gen ((L l):rest) acc = gen rest $ infixE (Just acc) (varE $ mkName "++") (Just $ litE $ stringL l) 
gen (S:rest) acc = do
  name <- newName "a"
  lam1E (varP name) $ gen rest $ infixE (Just acc) (varE $ mkName "++") (Just $ varE name)
gen (D:rest) acc = do
  name <- newName "a"
  lam1E (varP name) $ gen rest $ infixE (Just acc) (varE $ mkName "++") (Just $ appE (varE $ mkName "show") (varE name))


printf :: String -> Q Exp
printf str = gen (parse str) [| "" |]