data T = T
f, g :: a -> T -> (b, T)

f x t = (2 * x, t)
g x t = (2 + x, t)

bind :: (a -> T -> (b, T)) -> (T -> (a, T)) -> (T -> (b, T))

