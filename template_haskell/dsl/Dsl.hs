module Dsl (code) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

code = QuasiQuoter { quoteExp = parseExp, quotePat =  parsePat }

parseExp :: String -> ExpQ
parseExp s = stringE s

parsePat :: String -> PatQ
parsePat = undefined
