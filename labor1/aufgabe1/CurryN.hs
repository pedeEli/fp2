module CurryN where

import Language.Haskell.TH
import Control.Monad


curryN :: Quote m => Int -> m Exp
curryN n
  | n < 2 = error "n should be greater or equal to 2"
  | otherwise = lamE (map varP $ func : names) $ appE (varE func) (tupE $ map varE names)
  where names = map (\n -> mkName $ 'a' : show n) [1..n]
        func = mkName "f"


genCurries :: Int -> Q [Dec]
genCurries n
  | n < 2 = error "n should be greater or equal to 2"
  | otherwise = mapM f [2..n]
  where
    f :: Quote m => Int -> m Dec
    f n = valD (varP name) (normalB $ curryN n) []
      where name = mkName $ "curry" ++ show n
      