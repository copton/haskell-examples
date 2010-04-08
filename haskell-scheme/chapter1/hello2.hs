module Main where
import System.Environment

main :: IO ()
main = do
	putStrLn ("enter name: ")
	name <- getLine
	putStrLn ("Hello, " ++ name)
