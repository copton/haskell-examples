module Sel where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

sel :: Int -> Int -> ExpQ
sel i n = [| \x -> $(caseE [| x |] [alt]) |]
    where alt :: MatchQ
          alt = match pat (normalB rhs) []
 
          pat :: PatQ
          pat = tupP (map (varP . mkName) as)
 
          rhs :: ExpQ
          rhs = varE $ mkName $ (as !! (i -1)) -- !! is 0 based
 
          as :: [String]
          as = ["a" ++ show i | i <- [1..n] ]
