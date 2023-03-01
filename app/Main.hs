module Main where

import System.Environment (getArgs)
import System.Random
import MarkovText

main :: IO ()
main = do
    args <- getArgs
    texts <- mapM readFile args
    rng <- newStdGen
    putStrLn . takeUntilFullStop $ generate rng texts

takeUntilFullStop :: String -> String
takeUntilFullStop (char:text)
    | char `elem` ".?!" = [char]
    | otherwise = char : takeUntilFullStop text
