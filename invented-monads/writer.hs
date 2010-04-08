f,g :: Float -> (Float, String)

f x = (x+2, "f called")
g x = (x*2, "g called")

bind :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind f (x,s) = let (x',s') = f x in (x', s ++ " " ++ s')

action :: Float -> (Float, String)
--action x = (bind f)(g(x))
action = bind f . g

main :: IO ()
main = putStrLn (show (action 2))
