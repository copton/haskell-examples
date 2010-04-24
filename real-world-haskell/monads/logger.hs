type Log = [String]
data Logger a = Logger { execLogger :: (a, Log) }

record :: String -> Logger ()
record s = Logger ((), [s])

getLog :: Logger a -> Log
getLog (Logger (_, l)) = l

instance Monad Logger where
    m >>= f = let (a, w) = execLogger m
                  n = f a
                  (b, x) = execLogger n
                  in Logger (b, w ++ x)

--    m >> f = m >>= \_ -> f

    return a = Logger (a, [])

f :: (Num a) => a -> Logger a
f x = do
    record "f called"
    return (x + 1)

g :: (Num a) => a -> Logger a
g x = do
    record "g called"
    return (x + 2)

exec :: (Num a) => Logger a
exec = do
    x <- f 1
    y <- g x
    return y

main = putStrLn $ show $ getLog $ exec
