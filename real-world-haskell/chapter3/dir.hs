data Direction = DirLeft | DirRight | DirStraight deriving (Show)

data Point = Point {x :: Int, y :: Int}

angle :: Point -> Point -> Float
angle a b = atan (fromIntegral(dy) / fromIntegral(dx))
   where dx = (x b) - (x a)
         dy = (y b) - (y a) 

dir :: Point -> Point -> Point -> Direction
dir a b c | alpha == beta = DirStraight
          | alpha < beta = DirLeft
          | otherwise = DirRight
              where alpha = angle a b
                    beta = angle b c
    
toDegree :: Float -> Float
toDegree s = s * 360 / (2 * pi)

toPoint :: [String] -> [Point]
toPoint [] = []
toPoint (x:y:rest) = (Point (read x) (read y)) : (toPoint rest)
toPoint _ = error "not enough coordinates given"

directions :: [Point] -> [Direction]
directions (a:b:c:[]) = [dir a b c]
directions (a:b:c:rest) = dir a b c : (directions (b : c : rest))
directions _ = error "not enough points given"

act :: String -> String
act input = show $ directions $ toPoint $ words input

main :: IO ()
main = interact act
