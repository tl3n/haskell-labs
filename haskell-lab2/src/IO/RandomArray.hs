module IO.RandomArray
    (
      generateRandomArray
    , promptRandomConfig
    ) where

import System.Random (randomRIO)

import IO.Prompt (promptWithDefault)
import Types (RandomArrayConfig (..), defaultRandomConfig)

generateRandomArray :: RandomArrayConfig -> IO [Int]
generateRandomArray cfg =
    traverse (\_ -> randomRIO (racMinVal cfg, racMaxVal cfg)) [1 .. racSize cfg]

promptRandomConfig :: IO RandomArrayConfig
promptRandomConfig = do
    size   <- promptWithDefault "Enter array size: " (racSize defaultRandomConfig)
    minVal <- promptWithDefault "Enter minimum value (default 0): " (racMinVal defaultRandomConfig)
    maxVal <- promptWithDefault "Enter maximum value (default 100): " (racMaxVal defaultRandomConfig)
    pure RandomArrayConfig
        { racSize   = size
        , racMinVal = minVal
        , racMaxVal = maxVal
        }

