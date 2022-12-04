{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.List.Split
import Data.Range
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  ( print
      . length
      . filter overlaps
      . map (bimap makeRange makeRange . unsafeTuples . splitOn ",")
      . lines
    )
    input

makeRange :: String -> Range Int
makeRange xs = read lhs +=+ read rhs
  where
    [lhs, rhs] = splitOn "-" xs

overlaps :: (Range Int, Range Int) -> Bool
overlaps (x, y) = isEmpty (difference [x] [y]) || isEmpty (difference [y] [x])

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False
