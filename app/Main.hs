module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = getArgs >>= mapM readFile >>= generate >>= putStrLn
