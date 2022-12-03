{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents

  ( print
      . sum
      . concatMap (S.toList . intersections . map (S.fromList . toInts))
      . chunksOf 3
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

intersections :: (Ord a) => [S.Set a] -> S.Set a
intersections [] = S.empty
intersections [s1] = s1
intersections [s1, s2] = S.intersection s1 s2
intersections (s1 : s2 : ss) = intersections (S.intersection s1 s2 : ss)
