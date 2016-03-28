module Main where

import System.Environment (getArgs)
import System.Random
import Lib

main :: IO ()
main = do
    args <- getArgs
    texts <- mapM readFile args
    rng <- newStdGen
    putStrLn . take 100 $ generate rng texts
