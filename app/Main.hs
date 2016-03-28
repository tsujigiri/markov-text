module Main where

import System.Environment (getArgs)
import System.Random
import MarkovText

main :: IO ()
main = do
    args <- getArgs
    texts <- mapM readFile args
    rng <- newStdGen
    putStrLn . take 100 $ generate rng texts
