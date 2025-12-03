{- |
The algorithm works by:

1. Padding the input to a power of 2 (required by Batcher's algorithm)
2. Recursively sorting halves in parallel
3. Merging using the odd-even merge network
4. Removing padding from the result
-}
module Sort.Batcher
    (
      parallelBatcherSort
    ) where

import Control.Parallel.Strategies (Eval, rparWith, rseq, runEval, rdeepseq)
import Control.DeepSeq (NFData)
import Data.Vector (Vector, (!))
import Data.Bits ((.&.))
import qualified Data.Vector as V

parallelBatcherSort :: (Ord a, NFData a) => Int -> [a] -> [a]
parallelBatcherSort _ [] = []
parallelBatcherSort k xs =
    let originalLen = length xs
        paddedVec   = padToPowerOfTwo (V.fromList xs)
        sortedVec   = runEval (sortRecursive (max 1 k) paddedVec)
    in V.toList (V.take originalLen sortedVec)

--------------------------------------------------------------------------------
-- Vector Utilities
--------------------------------------------------------------------------------

padToPowerOfTwo :: Ord a => Vector a -> Vector a
padToPowerOfTwo v
    | V.null v     = V.empty
    | isPowerOf2 n = v
    | otherwise    = v V.++ V.replicate paddingSize padValue
  where
    n           = V.length v
    nextPow2    = findNextPowerOf2 n
    paddingSize = nextPow2 - n
    padValue    = V.maximum v

isPowerOf2 :: Int -> Bool
isPowerOf2 n = n > 0 && (n .&. (n - 1)) == 0

findNextPowerOf2 :: Int -> Int
findNextPowerOf2 n
    | n <= 0    = 1
    | otherwise = 2 ^ ceiling (logBase 2 (fromIntegral n :: Double))

--------------------------------------------------------------------------------
-- Recursive Parallel Sort
--------------------------------------------------------------------------------

sortRecursive :: (Ord a, NFData a) => Int -> Vector a -> Eval (Vector a)
sortRecursive _ v | V.length v <= 1 = pure v
sortRecursive k v = do
    let (left, right) = V.splitAt (V.length v `div` 2) v
    (sortedLeft, sortedRight) <- sortHalves k left right
    mergeRecursive k (sortedLeft V.++ sortedRight)

sortHalves :: (Ord a, NFData a) => Int -> Vector a -> Vector a -> Eval (Vector a, Vector a)
sortHalves 1 left right = do
    sortedLeft  <- sortRecursive 1 left
    sortedRight <- sortRecursive 1 right
    pure (sortedLeft, sortedRight)
sortHalves k left right = do
    let k1 = k `div` 2
        k2 = k - k1
    sortedLeft  <- rparWith rdeepseq (runEval (sortRecursive k1 left))
    sortedRight <- rparWith rdeepseq (runEval (sortRecursive k2 right))
    _ <- rseq sortedLeft
    _ <- rseq sortedRight
    pure (sortedLeft, sortedRight)

--------------------------------------------------------------------------------
-- Batcher's Odd-Even Merge
--------------------------------------------------------------------------------

mergeRecursive :: (Ord a, NFData a) => Int -> Vector a -> Eval (Vector a)
mergeRecursive _ v | V.length v <= 1 = pure v
mergeRecursive _ v | V.length v == 2 = pure (compareSwap v)
mergeRecursive k v = do
    let (evens, odds) = extractEvenOdd v
    (mergedEvens, mergedOdds) <- mergeHalves k evens odds
    pure (interleaveAndExchange mergedEvens mergedOdds)

compareSwap :: Ord a => Vector a -> Vector a
compareSwap v
    | e1 <= e2  = v
    | otherwise = V.fromListN 2 [e2, e1]
  where
    e1 = v ! 0
    e2 = v ! 1

extractEvenOdd :: Vector a -> (Vector a, Vector a)
extractEvenOdd v = (evens, odds)
  where
    half     = V.length v `div` 2
    (a, b)   = V.splitAt half v
    numEvens = (half + 1) `div` 2
    numOdds  = half `div` 2
    evens    = V.generate numEvens (\i -> a ! (2 * i))
            V.++ V.generate numEvens (\i -> b ! (2 * i))
    odds     = V.generate numOdds (\i -> a ! (2 * i + 1))
            V.++ V.generate numOdds (\i -> b ! (2 * i + 1))

mergeHalves :: (Ord a, NFData a) => Int -> Vector a -> Vector a -> Eval (Vector a, Vector a)
mergeHalves 1 evens odds = do
    mergedEvens <- mergeRecursive 1 evens
    mergedOdds  <- mergeRecursive 1 odds
    pure (mergedEvens, mergedOdds)
mergeHalves k evens odds = do
    let k1 = k `div` 2
        k2 = k - k1
    mergedEvens <- rparWith rdeepseq (runEval (mergeRecursive k1 evens))
    mergedOdds  <- rparWith rdeepseq (runEval (mergeRecursive k2 odds))
    _ <- rseq mergedEvens
    _ <- rseq mergedOdds
    pure (mergedEvens, mergedOdds)

interleaveAndExchange :: Ord a => Vector a -> Vector a -> Vector a
interleaveAndExchange evens odds = V.generate totalLen elementAt
  where
    totalLen    = V.length evens + V.length odds
    interleaved = V.generate totalLen $ \i ->
        if even i then evens ! (i `div` 2) else odds ! (i `div` 2)

    elementAt 0 = interleaved ! 0
    elementAt i
        | odd i && i < totalLen - 1 = min (interleaved ! i) (interleaved ! (i + 1))
        | even i && odd (i - 1)     = max (interleaved ! (i - 1)) (interleaved ! i)
        | otherwise                 = interleaved ! i

