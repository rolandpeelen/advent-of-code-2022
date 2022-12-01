{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Lib
import Data.List

main :: IO ()
main = do
  input <- getContents
  (print . sum . take 3 . sortBy (flip compare) . fn . lines) input

fn :: [String] -> [ Int ] 
fn xs = go 0 [] xs
  where 
    go current acc [] = acc
    go current acc ("":ys) = go 0  (acc ++ [current]) ys
    go current acc (y:ys) = go (current + (read y)) acc ys
