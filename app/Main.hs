module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Random
import Lib

main :: IO ()
main = do
    args <- getArgs
    texts <- mapM readFile args
    let wordLists = map processWords texts
    let wordMap = foldl addToWordMap Map.empty wordLists
    rng <- newStdGen
    putStrLn $ joinWords $ take 140 $ generateText Nothing wordMap rng
