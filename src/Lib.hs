module Lib (
    addToWordMap,
    generateText,
    processWords,
    joinWords
    ) where

import qualified Data.Map as Map
import System.Random

type WordMap = Map.Map (String, String) [String]

addToWordMap :: WordMap -> [String] -> WordMap
addToWordMap oldMap (w1:w2:w3:wordList)
    | endsWithPunctuation w2 = addToWordMap oldMap (w3:wordList)
    | otherwise = addToWordMap newMap (w2:w3:wordList)
    where newMap = Map.insertWith (++) (w1, w2) [w3] oldMap

addToWordMap oldMap _ = oldMap

generateText :: Maybe (String, String) -> WordMap -> StdGen -> [String]
generateText Nothing wordMap rng = w1:w2:(generateText nextKey wordMap newRng)
    where firstWords = Map.filterWithKey (\k _ -> (head . fst) k `elem` ['A'..'Z']) wordMap
          keyCount = length . Map.keys $ firstWords
          (randIndex, newRng) = randomR (0, keyCount - 1) rng :: (Int, StdGen)
          (w1, w2) = Map.keys firstWords !! randIndex
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

endsWithPunctuation :: String -> Bool
endsWithPunctuation string = last string `notElem` ['a'..'z'] ++ ['A'..'Z']

processWords :: String -> [String]
processWords (char:text)
    | isWordChar char = word:(processWords (drop (length word - 1) text))
    | isWhitespace char = processWords $ dropWhile isWhitespace text
    | isPunctuation char = [char]:(processWords text)
    | isIgnored char = processWords text
    | otherwise = error $ [char] ++ " not known"
    where word = char:(takeWhile isWordChar text)

processWords [] = []

isWordChar :: Char -> Bool
isWordChar = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'-$~%&/=*#+\\@")

isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t")

isPunctuation :: Char -> Bool
isPunctuation = (`elem` ".,!?;:")

isIgnored :: Char -> Bool
isIgnored = (`elem` "\n\"<>()|[]_{}")

joinWords :: [String] -> String
joinWords (word1:word2:words)
    | length word2 == 1 && isPunctuation (head word2) = word1 ++ word2 ++ " " ++ joinWords words
    | otherwise = word1 ++ " " ++ joinWords (word2:words)

joinWords [word] = word
joinWords [] = ""
