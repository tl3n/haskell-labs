{-
  Parallel Batcher's Odd-Even Mergesort in Haskell
  
  To run (e.g., with 4 threads):
  cabal run haskell-lab2 -- +RTS -N4 -RTS
-}

import Control.Parallel.Strategies
import qualified Data.Vector as V
import Data.Vector (Vector)
import System.IO (stdout, hFlush)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Data.Char (toLower)

-- | Generate a random array of given size
generateRandomArray :: Int -> Int -> Int -> IO [Int]
generateRandomArray size minVal maxVal =
    sequence $ replicate size (randomRIO (minVal, maxVal))

-- | Main entry point
main :: IO ()
main = do
    -- 1. Get k (number of threads)
    putStr "Enter number of threads (k): "
    hFlush stdout
    kStr <- getLine
    -- Default to 1 thread if input is invalid
    let k = fromMaybe 1 (readMaybe kStr)
    
    -- 2. Ask user if they want to generate random array or input manually
    putStr "Generate random array? (y/n): "
    hFlush stdout
    choice <- getLine
    
    nums <- if map toLower choice == "y" || map toLower choice == "yes"
        then do
            -- Generate random array
            putStr "Enter array size: "
            hFlush stdout
            sizeStr <- getLine
            let size = fromMaybe 10 (readMaybe sizeStr)
            
            putStr "Enter minimum value (default 0): "
            hFlush stdout
            minStr <- getLine
            let minVal = fromMaybe 0 (readMaybe minStr)
            
            putStr "Enter maximum value (default 100): "
            hFlush stdout
            maxStr <- getLine
            let maxVal = fromMaybe 100 (readMaybe maxStr)
            
            putStrLn $ "Generating random array of size " ++ show size ++ 
                       " with values in range [" ++ show minVal ++ ", " ++ show maxVal ++ "]..."
            generateRandomArray size minVal maxVal
        else do
            -- Manual input
            putStr "Enter numbers separated by spaces: "
            hFlush stdout
            line <- getLine
            return (map read (words line) :: [Int])
    
    putStrLn $ "Original: " ++ show nums
    putStrLn $ "Using " ++ show k ++ " thread(s)..."
    
    -- 3. Run the sort
    -- Note: The +RTS -Nk flag at runtime actually sets the *available*
    -- capabilities. Our 'k' variable controls how we *divide* the work.
    let sorted = parallelBatcherSort k nums
    
    putStrLn $ "Sorted:   " ++ show sorted

-- | Public-facing sort function.
-- | Handles padding, running the parallel computation, and unpadding.
parallelBatcherSort :: (Ord a) => Int -> [a] -> [a]
parallelBatcherSort k xs
  | null xs   = []
  | otherwise =
    let originalLength = length xs
        vec            = V.fromList xs
        -- 1. Pad vector to a power of 2
        paddedVec      = padVector vec
    in
    -- 2. Run the parallel sort using runEval
    let sortedPaddedVec = runEval (sortR (max 1 k) paddedVec)
    -- 3. Unpad by taking the original length and convert back to list
     in V.toList (V.take originalLength sortedPaddedVec)

-- | Pads a vector to the next power of 2.
-- | Batcher's algorithm requires this.
padVector :: (Ord a) => Vector a -> Vector a
padVector v =
  let n = V.length v
      -- Find the next power of 2 (handle n=0 case for log)
      nextPow = if n == 0 then 0 else 2 ^ (ceiling (logBase 2 (fromIntegral n :: Double)) :: Int)
  in
  if n == 0
  then V.empty
  else if n == nextPow
  then v
  else
      -- Pad with the maximum element so padded values end up at the end after sorting
      let padVal    = V.maximum v
          padding   = V.replicate (nextPow - n) padVal
      in v V.++ padding


-- | The PARALLEL RECURSIVE SORTER.
-- | Takes 'k' available threads and a vector 'v'.
sortR :: (Ord a) => Int -> Vector a -> Eval (Vector a)
sortR k v =
  let n = V.length v
  in
  if n <= 1
  then return v
  else
    let 
        half  = n `div` 2
        (v1, v2) = V.splitAt half v
    in
    -- Check if we should run sequentially
    if k == 1
    then do
      -- Sequential execution (but still within Eval monad)
      v1_sorted <- sortR 1 v1
      v2_sorted <- sortR 1 v2
      mergeR 1 (v1_sorted V.++ v2_sorted)
    else do
      -- Parallel execution
      -- Split threads between the two sub-problems
      let k1 = k `div` 2
          k2 = k - k1
          
      -- Spark the first half (rpar)
      v1_eval <- rpar (sortR k1 v1)
      -- Spark the second half (rpar)
      v2_eval <- rpar (sortR k2 v2)
      
      -- Wait for the first half to finish (rseq) and extract the result
      v1_sorted_eval <- rseq v1_eval
      v1_sorted <- v1_sorted_eval
      -- Wait for the second half to finish (rseq) and extract the result
      v2_sorted_eval <- rseq v2_eval
      v2_sorted <- v2_sorted_eval
      
      -- Now, merge the two sorted results (can also be parallel)
      mergeR k (v1_sorted V.++ v2_sorted)

-- | The PARALLEL RECURSIVE MERGER (Batcher's Odd-Even Merge).
-- | Takes 'k' available threads and a vector 'v' (assumed to be two sorted halves).
-- | The vector v should be of length n, where the first n/2 and last n/2 are sorted.
mergeR :: (Ord a) => Int -> Vector a -> Eval (Vector a)
mergeR k v =
  let n = V.length v
  in
  if n <= 1
  then return v
  else if n == 2
  then
    -- Base case: compare and swap if needed
    let e1 = v V.! 0
        e2 = v V.! 1
    in return $ if e1 <= e2 then v else V.fromList [e2, e1]
  else
    let half = n `div` 2
        -- Split into two sorted halves: A (first half) and B (second half)
        (a, b) = V.splitAt half v
        -- Extract odd-indexed elements from A and B, then concatenate
        -- For a sequence of length half, there are half `div` 2 odd-indexed elements
        numOdds = half `div` 2
        a_odds = V.generate numOdds $ \i -> a V.! (2 * i + 1)
        b_odds = V.generate numOdds $ \i -> b V.! (2 * i + 1)
        odds = a_odds V.++ b_odds
        -- Extract even-indexed elements from A and B, then concatenate
        -- For a sequence of length half, there are (half + 1) `div` 2 even-indexed elements
        numEvens = (half + 1) `div` 2
        a_evens = V.generate numEvens $ \i -> a V.! (2 * i)
        b_evens = V.generate numEvens $ \i -> b V.! (2 * i)
        evens = a_evens V.++ b_evens
    in
    -- Check if we should run sequentially
    if k == 1
    then do
      -- Sequential execution: merge odds and evens separately
      odds_merged  <- mergeR 1 odds
      evens_merged <- mergeR 1 evens
      -- Interleave and perform final compare-exchange
      return $ finalCompareExchange evens_merged odds_merged
    else do
      -- Parallel execution
      let k1 = k `div` 2
          k2 = k - k1
          
      -- Spark the odd merge
      odds_eval <- rpar (mergeR k1 odds)
      -- Spark the even merge
      evens_eval <- rpar (mergeR k2 evens)
      
      -- Wait for results and extract them
      odds_merged_eval <- rseq odds_eval
      odds_merged <- odds_merged_eval
      evens_merged_eval <- rseq evens_eval
      evens_merged <- evens_merged_eval
      
      -- Final compare-exchange step
      return $ finalCompareExchange evens_merged odds_merged

-- | Final compare-exchange step in Batcher's odd-even merge.
-- | Interleaves two sorted sequences (evens first, then odds) and performs compare-exchange operations.
-- | Pattern: [e0, o0, e1, o1, e2, o2, ...]
finalCompareExchange :: (Ord a) => Vector a -> Vector a -> Vector a
finalCompareExchange evens odds =
  let n_evens = V.length evens
      n_odds = V.length odds
      n = n_evens + n_odds
      -- Interleave evens and odds: [e0, o0, e1, o1, ...]
      interleaved = V.generate n $ \i ->
        if even i
        then evens V.! (i `div` 2)
        else odds V.! (i `div` 2)
  in
  -- Perform compare-exchange: compare pairs (2i+1, 2i+2) for i=0,1,2,...
  -- This means we compare positions: (1,2), (3,4), (5,6), ...
  -- For each pair (i, i+1) where i is odd:
  --   position i gets min(interleaved[i], interleaved[i+1])
  --   position i+1 gets max(interleaved[i], interleaved[i+1])
  V.generate n $ \i ->
    if i == 0
    then
      -- First element is unchanged
      interleaved V.! i
    else if odd i && i < n - 1
    then
      -- Odd position: take minimum of pair (i, i+1)
      let e1 = interleaved V.! i
          e2 = interleaved V.! (i + 1)
      in min e1 e2
    else if even i && odd (i - 1)
    then
      -- Even position following an odd position: take maximum of pair (i-1, i)
      let e1 = interleaved V.! (i - 1)
          e2 = interleaved V.! i
      in max e1 e2
    else
      -- Other positions (shouldn't happen in standard case, but handle gracefully)
      interleaved V.! i