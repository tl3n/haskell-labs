module TextProcessor.Utils
    ( normalizeWhitespace
    , isValidChar
    , cleanWord
    , extractWords
    , groupByFirstLetter
    ) where

import Data.List (groupBy)
import Data.Char (isAlpha, toLower)

import TextProcessor.Types (WordString, Symbol)

normalizeWhitespace :: String -> String
normalizeWhitespace text = unwords $ words text

isValidChar :: Symbol -> Bool
isValidChar c = isAlpha c || c == '-' || c == '\''

cleanWord :: String -> WordString
cleanWord = filter isAlpha

extractWords :: String -> [WordString]
extractWords text = 
    filter (not . null)
    $ map (map toLower . cleanWord)
    $ words
    $ normalizeWhitespace text

groupByFirstLetter :: [WordString] -> [[WordString]]
groupByFirstLetter = groupBy (\w1 w2 -> head w1 == head w2)

