module Quoter.Haskell where

import Data.List (find, isInfixOf)
import Data.Maybe

generateNames :: String -> String -> [String]
generateNames n inp = map ((prefix ++) . show) [0..]
  where
    -- Find a variable prefix that's not used anywhere in the input.
    prefix = fromJust $ find (not . (`isInfixOf` inp)) $ map (concat . flip replicate n) [1..]
