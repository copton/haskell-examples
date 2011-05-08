-- import {{{1
import Control.Exception (assert)
import Data.List (transpose, intersperse, splitAt)
import Control.Monad.Error

-- types {{{1
data Color = Empty | Filled | Unknown deriving (Eq)

isFilled Filled = True
isFilled _ = False
isEmpty Empty = True
isEmpty _ = False
isUnknown Unknown = True
isUnknown _ = False

data Block = Block Color Int deriving (Show)

type Constraints = [Int]

type Placement = [Block]

type Coloring = [Color]

type Field = (Int, Int)

type Selection = [Field]

data Board = Board [Coloring] [[Coloring]] [[Coloring]] deriving (Show)

data Solution = Solution [Coloring]

data Puzzle = Puzzle Int Int [Constraints] [Constraints]

-- visualization {{{1
instance Show Solution where
	show (Solution colorings) = 
		let 
			sections = take (length $ head colorings) (cycle "-") 
			header = "+" ++ sections ++ "+"
			body = map showColoring colorings	
		in 
			concat $ intersperse "\n" $ header : body ++ [header]

showColoring coloring = concat $ "|" : map show coloring ++ ["|"]

instance Show Color where
	show Empty = "_"
	show Filled = "X"
	show Unknown = "?"

-- combinatorics {{{1
-- list all possible placements for given constraints
listPossibilities :: Int -> Constraints -> [Placement]
listPossibilities size [] = [[Block Empty size]]
listPossibilities size constraint = 
	let 
		minSpace = (sum constraint) + (length constraint) - 1
		withSpace = placeEmptyBlock size minSpace constraint
		withoutSpace = placeFilledBlock size minSpace constraint
	in
		withoutSpace ++ withSpace
	
-- find all possibilities to place an empty block of given size in the available space
placeEmptyBlock :: Int -> Int -> Constraints -> [Placement]
placeEmptyBlock space minSpace constraint = do
	size <- [1..space-minSpace]
	let space' = space - size
	map ((Block Empty size):) $ placeFilledBlock space' minSpace constraint

-- find all possibilities to place a filled block of given size in the available space
placeFilledBlock :: Int -> Int -> Constraints -> [Placement]
placeFilledBlock space minSpace [size] 
	| space == size = [[Block Filled size]]
	| otherwise = [[Block Filled size, Block Empty (space - size)]]

placeFilledBlock space minSpace (size:rest) = 
	let
		space' = space-size
		minSpace' = minSpace - size - 1
	in
		map ((Block Filled size):) $ placeEmptyBlock space' minSpace' rest

-- find the list of possible colorings for each column/row
possibleColorings :: Int -> [Constraints] -> [[Coloring]]
possibleColorings size constraints = map place constraints
	where
		place :: Constraints -> [Coloring]
		place constraint = map convert $ listPossibilities size constraint

-- convert a placement into a coloring
convert :: Placement -> Coloring
convert [] = []
convert ((Block Empty size):rest) = replicate size Empty ++ convert rest
convert ((Block Filled size):rest) = replicate size Filled ++ convert rest

-- solver {{{1

solve :: Board -> Solution
solve board@(Board coloring _ _)
	| completed board = Solution coloring
	| otherwise = solve $ converge board

converge :: Board -> Board
converge (Board coloring columns rows) =
	let
		coloring' = transpose $ map intersectColorings rows
		columns' = map filterColorings $ zip columns coloring'
		coloring'' = transpose $ map intersectColorings columns'
		rows' = map filterColorings $ zip rows coloring''
	in
		Board coloring'' columns' rows'

-- check if solution is complete
completed :: Board -> Bool
completed (Board coloring _ _) = not $ or $ map (any isUnknown) coloring

-- filter out all colorings that are no possible extensions of the current one
filterColorings :: ([Coloring], Coloring)-> [Coloring]
filterColorings (colorings, current) = filter (validColoring current) colorings

-- check if a new coloring would be a valid extension of the current one
validColoring :: Coloring -> Coloring -> Bool
validColoring existing new = all fit (zip existing new)
	where
		fit (Unknown, _) = True
		fit (Filled, Filled) = True
		fit (Empty, Empty) = True
		fit (_, _) = False

-- determine the fields that all possible colorings agree upon
intersectColorings :: [Coloring] -> Coloring
intersectColorings colorings = map intersect $ transpose colorings
	where
		intersect (candidate:rest) = 
			if all (==candidate) rest 
				then candidate 
				else Unknown

-- input {{{1
-- create a new board
newBoard :: Puzzle -> Either String Board
newBoard puzzle = do
	(Puzzle xsize ysize columns rows) <- checkPuzzle puzzle
	let initial = replicate ysize $ replicate xsize Unknown
	let columns' = possibleColorings xsize columns
	let rows' = possibleColorings ysize rows
	return $ Board initial columns' rows'

-- checkPuzzle
checkPuzzle :: Puzzle -> Either String Puzzle
checkPuzzle puzzle@(Puzzle xsize ysize columns rows) = do
	validLength "columns" ysize columns
	validLength "rows " xsize rows
	validConstraints ysize columns
	validConstraints xsize rows
	return puzzle

minSize :: Constraints -> Int
minSize cs = sum cs + length cs - 1

validLength :: String -> Int -> [Constraints] -> Either String ()
validLength typ size css
	| size < len = fail $ "not enough " ++ typ ++ ": " ++ show len
	| size > len = fail $ "too many " ++ typ ++ ": " ++ show len
	| otherwise = return ()
	where len = length css

checkConstraints :: Int -> Constraints -> Either String ()
checkConstraints max cs
	| max < minSize cs = fail $ "invalid constraints: " ++ show cs
	| otherwise = return ()

validConstraints :: Int -> [Constraints] -> Either String ()
validConstraints max css = mapM_ (checkConstraints max) css

-- run the solver
main :: IO ()
main = putStrLn $ show $ solve $ runBoard $ newBoard puzzle

runBoard (Left s) = error s
runBoard (Right b) = b

-- puzzles {{{1
puzzle = meer
-- bird {{{2
-- +---------------+
-- |XXXXXXXXXXXXXXX|
-- |XXXXXXXXXX   XX|
-- |X XXXXXXX     X|
-- |X           X X|
-- |X              |
-- |X      XXX    X|
-- |X    XXXXX   XX|
-- |XX  XXXXXX  XXX|
-- |XXX XXXXX   XXX|
-- |XXXX       XXX |
-- |XXXXXX X XXX XX|
-- |XXXXXX X XXXXX |
-- |XXXXX      XXXX|
-- |XXXXXXXXXXXXXXX|
-- |XXXXXXXXXXXXXXX|
-- +---------------+
bird :: Puzzle
bird = Puzzle xsize ysize columns rows
	where
		xsize = 15
		ysize = 15
		columns = [
				[15], [2,8], [3,7], [3,6], [3,2,5],
				[3,3,2,2], [3,3,2], [3,4,2,2], [3,4,2], [2,3,2,2],
				[1,2,2], [1,6], [1,1,3,4], [2,9], [4,4,1,3]
			]
		rows = [
				[15], [10,2], [1,7,1], [1,1,1], [1],
				[1,3,1], [1,5,2], [2,6,3], [3,5,3], [4,3],
				[6,1,3,2], [6,1,5], [5,4], [15], [15]
			]

-- larger {{{2
-- +--------------------+
-- |XX   XX XXXXXXX     |
-- |X   XXX XXX         |
-- |X   XXX       XX  XX|
-- |X   XXXX      XXXXXX|
-- |X    XXXXX   XXXXXXX|
-- |XXXX  XXXXX   XXXXXX|
-- |XXXX  XXXXX  XXXXX  |
-- |XXXX  XXXXX         |
-- | XXX XXXX XX        |
-- |XX XXXXXXX          |
-- |X   XXXXX  X        |
-- |    XXXX      XXX   |
-- |    XXXX    X  XXX  |
-- |    XXXXX   X  XXX  |
-- |       XXXXXX   XX  |
-- |       XXXXXX   XX  |
-- |        XXX     XXX |
-- |XX  X   XXX X    X  |
-- |XXXXXXXX    XXXXXXXX|
-- |XXXXXXXX   XXXXXXXXX|
-- +--------------------+
-- larger :: Puzzle
larger = Puzzle 20 20 columns rows
	where
		columns = [
				[8,2,3], [1,5,3], [4,2], [5,2], [3,5,3],
				[5,6,2], [14,2], [13,2], [2,7,5], [2,4,1,4],
				[2,4,4], [1,1,1,2,1], [1,4,3], [1,1,1,2], [1,5,1,2],
				[5,3,2], [4,6,2], [4,8], [4,1,2], [4,2]
			]
		rows = [
				[2,2,7], [1,3,3], [1,3,2,2], [1,4,6], [1,5,7],
				[4,5,6], [4,5,5], [4,5], [3,4,2], [2,7],
				[1,5,1], [4,3], [4,1,3], [5,1,3], [6,2],
				[6,2], [3,3], [2,1,3,1,1], [8,8], [8,9]
			]

-- meer {{{2
-- +-------------------------+
-- |XXXX____XXX_XXXXXXXX__XXX|
-- |XX______XXX_XXXXXXXX__XXX|
-- |________XXX_XXX_XXXXX_XXX|
-- |_____________XX_XXXXXXXX_|
-- |___XX________XXXXXXXXXXX_|
-- |X_XXXX________X_XXXXXXXXX|
-- |XXXXXXXX______XXXXXXXXXXX|
-- |___X______XXXXXXXXXXXXX__|
-- |_X_XXXXXXXXXXXXXXXXXXXX__|
-- |___XXXXXXXXXXX_____XXXX__|
-- |_XXXXXXXXXX_XX_____XXXXX_|
-- |X_XXXX__X___XXX_____XXXXX|
-- |XXXXXX__X____XX___XXX__XX|
-- |X___XX__X_________XXX____|
-- |XX____X__________XXX_____|
-- |XX_____________XXXX______|
-- |XXX____________XXXXX____X|
-- |XX_____________XXXXXX__XX|
-- |X_____________XXX_X_X_X_X|
-- |____________X_XX______X_X|
-- |__X____X_XXXXX________XXX|
-- |__X__X_X_XXXXX_____X___XX|
-- |__XXX_XXXXXXX______X_XXX_|
-- |__XXXXXXXX_______X___XXXX|
-- |___XXXXXX_____XXXX___XXXX|
-- +-------------------------+
meer :: Puzzle
meer = Puzzle xsize ysize columns rows
	where
		xsize = 25
		ysize = 25
		columns = [
				[2,2,8],[2,1,1,1,1,4],[1,2,3,1,4],[1,9,3],[3,6,3],
				[2,6,1,2],[1,3,1,3],[1,3,5],[3,6,3],[3,3,4],
				[3,4,3],[3,3],[3,5,4],[5,6,2],[9,2,2,1],
				[2,1,3,5,1],[9,4,1],[9,4,2],[9,7],[11,3,2,2],
				[12,2],[9,3],[12,3,3],[7,3,1,5],[3,2,2,6,2]
			]
		rows = [
				[4,3,8,3],[2,3,8,3],[3,3,5,3],[2,8],[2,11],
				[1,4,1,9],[8,11],[1,13],[1,20],[11,4],
				[10,2,5],[1,4,1,3,5],[6,1,2,3,2],[1,2,1,3],[2,1,3],
				[2,4],[3,5,1],[2,6,2],[1,3,1,1,1,1],[1,2,1,1],
				[1,1,5,3],[1,1,1,5,1,2],[3,7,1,3],[8,1,4],[6,4,4]
			]

