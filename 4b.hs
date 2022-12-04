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
      . filter (uncurry rangesOverlap)
      . map (bimap makeRange makeRange . unsafeTuples . splitOn ",")
      . lines
    )
    input

makeRange :: String -> Range Int
makeRange xs = read lhs +=+ read rhs
  where
    [lhs, rhs] = splitOn "-" xs
