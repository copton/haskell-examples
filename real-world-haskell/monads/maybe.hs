import Prelude hiding (Maybe, Nothing, Just)

data Maybe a = Nothing 
               | Just a
                deriving (Show)

instance Monad Maybe where
    Nothing >>= _ = Nothing
    Just a >>= f = f a
    return x = Just x

f :: (Num a) => a -> a -> Maybe a
f x y | x /= 0 = Just (x + y)
      | otherwise = Nothing

exec :: (Num a) => a -> Maybe a
exec z = do
    x <- f z 2
    y <- f 3 x
    return y

main = do
    z <- getLine
    putStrLn $ show $ exec $ read z
