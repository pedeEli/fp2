module CurryN where

import Language.Haskell.TH
import Control.Monad


curry' :: ((a, b) -> c) -> a -> b -> c
curry' = \f a b -> f (a, b)

curryN :: Int -> Q Exp
curryN n = do
    let names = map (\n -> mkName $ 'a' : show n) [1..n]
        func = mkName "f"
    return $ LamE (map VarP $ func : names) $ AppE (VarE func) (TupE $ map (VarE) names)