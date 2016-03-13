module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Random

type WordMap = Map.Map (String, String) [String]

main :: IO ()
main = do
    args <- getArgs
    texts <- mapM readFile args
    let wordLists = map words texts
    let wordMap = foldl addToWordMap Map.empty wordLists
    rng <- newStdGen
    print $ take 10 $ generateText Nothing wordMap rng

addToWordMap :: WordMap -> [String] -> WordMap
addToWordMap oldMap (w1:w2:w3:wordList) = addToWordMap newMap (w2:w3:wordList)
    where newMap = Map.insertWith (++) (w1, w2) [w3] oldMap

addToWordMap oldMap _ = oldMap

generateText :: Maybe (String, String) -> WordMap -> StdGen -> [String]
generateText Nothing wordMap rng = w1:w2:(generateText nextKey wordMap newRng)
    where keyCount = length . Map.keys $ wordMap
          (randIndex, newRng) = randomR (0, keyCount - 1) rng :: (Int, StdGen)
          (w1, w2) = Map.keys wordMap !! randIndex
          nextKey = Just (w1, w2)

generateText (Just key) wordMap rng
    | words == Nothing = generateText Nothing wordMap rng
    | otherwise = nextWord:(generateText nextKey wordMap newRng)
    where words = Map.lookup key wordMap
          nextKey = if last lastWord `elem` ['a'..'z'] ++ ['A'..'Z'] then
                       Just (lastWord, nextWord)
                    else
                       Nothing
          lastWord = snd key
          Just wordCount = fmap length words
          (randIndex, newRng) = randomR (0, wordCount - 1) rng
          Just nextWord = fmap (!! randIndex) words
