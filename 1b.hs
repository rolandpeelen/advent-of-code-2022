{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  (print . sum . take 3 . sortBy (flip compare) . fn . lines) input

fn :: [String] -> [Int]
fn = go 0 []
  where
    go current acc [] = acc
    go current acc ("" : ys) = go 0 (acc ++ [current]) ys
    go current acc (y : ys) = go (current + read y) acc ys
