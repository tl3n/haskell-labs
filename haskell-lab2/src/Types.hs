module Types
    (
      RandomArrayConfig (..)
    , defaultRandomConfig
    ) where

data RandomArrayConfig = RandomArrayConfig
    { racSize   :: !Int
    , racMinVal :: !Int
    , racMaxVal :: !Int
    } deriving (Show, Eq)

defaultRandomConfig :: RandomArrayConfig
defaultRandomConfig = RandomArrayConfig
    { racSize   = 10
    , racMinVal = 0
    , racMaxVal = 100
    }

