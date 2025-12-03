module IO.Prompt
    (
      prompt
    , promptWithDefault
    , isAffirmative
    ) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt msg = do
    putStr msg
    hFlush stdout
    getLine


promptWithDefault :: Read a => String -> a -> IO a
promptWithDefault msg def = fromMaybe def . readMaybe <$> prompt msg


isAffirmative :: String -> Bool
isAffirmative s = map toLower s `elem` ["y", "yes"]

