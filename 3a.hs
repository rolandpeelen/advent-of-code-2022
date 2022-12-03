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
      . concatMap
        ( S.toList
            . uncurry S.intersection
            . bimap S.fromList S.fromList
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
