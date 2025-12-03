{- |
To run (e.g., with 4 threads):
cabal run haskell-lab2 -- +RTS -N4 -RTS
-}
module Main (main) where

import IO.Prompt (isAffirmative, prompt, promptWithDefault)
import IO.RandomArray (generateRandomArray, promptRandomConfig)
import Sort.Batcher (parallelBatcherSort)
import Types (RandomArrayConfig (..))

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = do
    threadCount <- promptWithDefault "Enter number of threads (k): " 1
    nums        <- getInputArray

    putStrLn $ "Original: " ++ show nums
    putStrLn $ "Using " ++ show threadCount ++ " thread(s)..."

    let sorted = parallelBatcherSort threadCount nums
    putStrLn $ "Sorted:   " ++ show sorted

--------------------------------------------------------------------------------
-- User Input
--------------------------------------------------------------------------------

getInputArray :: IO [Int]
getInputArray = do
    choice <- prompt "Generate random array? (y/n): "
    if isAffirmative choice
        then do
            cfg <- promptRandomConfig
            putStrLn $ unwords
                [ "Generating random array of size"
                , show (racSize cfg)
                , "with values in range"
                , "[" ++ show (racMinVal cfg) ++ "," ++ show (racMaxVal cfg) ++ "]..."
                ]
            generateRandomArray cfg
        else parseIntList <$> prompt "Enter numbers separated by spaces: "
  where
    parseIntList = map read . words
