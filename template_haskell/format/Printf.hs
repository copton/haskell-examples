module Printf where

-- Skeletal printf from the paper.
-- It needs to be in a separate module to the one where
-- you intend to use it.

-- Import some Template Haskell syntax
import Language.Haskell.TH
import Data.Maybe (catMaybes)

-- Describe a format string
data Format = D | S | L String

-- Parse a format string.  This is left largely to you
-- as we are here interested in building our first ever
-- Template Haskell program and not in building printf.
parse :: String -> [Format]
parse [] = []
parse ('%':'d':s) = D : parse s
parse ('%':'s':s) = S : parse s
parse s = L s' : parse s''
	where
		(s', s'') = span (/='%') s

-- Generate Haskell source code from a parsed representation
-- of the format string.  This code will be spliced into
-- the module which calls "pr", at compile time.
gen :: [Format] -> ExpQ
gen fs = lamE params [| concat $(listE (lgen fs names)) |]
    where
        names = map createName $ zip fs [1..]
        params = map varP $ catMaybes names
		
createName (D,i) = Just $ mkName $ "x" ++ show i
createName (S,i) = Just $ mkName $ "x" ++ show i
createName _ = Nothing

lgen :: [Format] -> [Maybe Name] -> [ExpQ]
lgen [] [] = []
lgen ((L s):fs) (Nothing:ns)= litE (stringL s) : lgen fs ns
lgen (D:fs) (Just name:ns) = [| show $(varE name) |] : lgen fs ns
lgen (S:fs) (Just name:ns) = varE name : lgen fs ns

-- Here we generate the Haskell code for the splice
-- from an input format string.
pr :: String -> Q Exp
pr s = gen (parse s)
