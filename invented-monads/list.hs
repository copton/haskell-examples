f,g :: Float -> [Float]

f x = [x, 2*x]
g x = [x, -x]

bind :: (Float -> [Float]) -> ([Float] -> [Float])
bind f l = concat $ map f l

action :: Float -> [Float]
action = bind f . g

main :: IO ()
main = putStrLn $ show $ action 2
