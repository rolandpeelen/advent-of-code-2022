{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Map as M
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  ( print
      . sum
      . concatMap
        ( map fst
            . M.toList
            . uncurry M.intersection
            . bimap countElems countElems
            . splitMiddle
            . map toInt
        )
      . lines
    )
    input

-- Ord gives a default offset for a char. We just offset it to the correct amounts
toInt :: Char -> Int
toInt x
  | isLower x = ord x - 96
  | otherwise = ord x - 38

splitMiddle :: [a] -> ([a], [a])
splitMiddle xs = splitAt s xs
  where
    l = length xs
    s = if even l then l `div` 2 else (l `div` 2) + 1

countElems :: (Ord a) => [a] -> M.Map a Int
countElems = M.fromListWith (+) . flip zip (repeat 1)
