module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ show( read (args !! 0) + read(args !! 1)))
