import Prelude hiding (filter)
import Control.Monad (liftM2)

data List a = List [a] deriving (Show)

instance Monad List where
    (List l) >>= f = List $ foldl (\acc (List l) -> acc ++ l) [] $ map f l
--    (List l) >>= f = foldl (liftM2 (++)) (List []) $ map f l
    return a = List [a]

filter :: Bool -> List a -> List a
filter True m = m
filter False _ = List []

exec = do
    x <- List [1..10]
    y <- List ['a'..'z']
    filter (x > 5) $ return (x,y)

main = putStrLn $ show $ exec
