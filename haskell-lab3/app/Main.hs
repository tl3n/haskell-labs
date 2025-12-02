module Main where

import System.IO
import Data.List (sort, nub, intercalate)

import TextProcessor

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    putStrLn "enter file name (filename.txt): "
    fileName <- getLine

    contents <- readFile fileName
    
    let allWords = extractWords contents
    let uniqueSortedWords = sort $ nub allWords
    let groupedWords = groupByFirstLetter uniqueSortedWords

    putStrLn "\n--- processing result ---\n"
    
    mapM_ printGroup groupedWords

printGroup :: [WordString] -> IO ()
printGroup groupOfWords = do
    let line = intercalate ", " groupOfWords
    putStrLn $ "    " ++ line
