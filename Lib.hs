module Lib (trim, unsafeTuples) where

import Data.Char (isSpace)
import Prelude

unsafeTuples :: [a] -> (a, a)
unsafeTuples [x, y] = (x, y)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
