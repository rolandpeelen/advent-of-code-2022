{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Set as S
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents

  ( print
      . sum
      . concatMap (S.toList . intersectMany . map (S.fromList . toInts))
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

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l = take n l : groupN n (drop n l)

intersectMany :: (Ord a) => [S.Set a] -> S.Set a
intersectMany [] = S.empty
intersectMany [s1] = s1
intersectMany [s1, s2] = S.intersection s1 s2
intersectMany (s1 : s2 : ss) = intersectMany (S.intersection s1 s2 : ss)
