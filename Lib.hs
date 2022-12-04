module Lib (unsafeTuples) where

import Prelude

unsafeTuples :: [a] -> (a, a)
unsafeTuples [x, y] = (x, y)
