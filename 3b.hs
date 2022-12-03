{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Map as M
import Lib
import Prelude

-- sum
-- concatMap
--( map fst
-- M.toList
-- intersectMany
-- map countElems
-- map toInt
--)
--
main :: IO ()
main = do
  input <- getContents

  ( print
      . sum
      . map fst
      . concatMap (M.toList . intersectMany . map (countElems . toInts))
      . groupN 3
      . lines
    )
    input

toInts :: String -> [Int]
toInts = map toInt

-- Ord gives a default offset for a char. We just offset it to the correct amounts
toInt :: Char -> Int
toInt x
  | isLower x = ord x - 96
  | otherwise = ord x - 38

countElems :: (Ord a) => [a] -> M.Map a Int
countElems = M.fromListWith (+) . flip zip (repeat 1)

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l = take n l : groupN n (drop n l)

intersectMany :: (Ord a) => [M.Map a b] -> M.Map a b
intersectMany [] = M.empty
intersectMany [m1] = m1
intersectMany [m1, m2] = M.intersection m1 m2
intersectMany (m1 : m2 : ms) = intersectMany (M.intersection m1 m2 : ms)
