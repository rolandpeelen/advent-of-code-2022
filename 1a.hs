{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Lib

main :: IO ()
main = do
  input <- getContents
  (print . fn . lines) input

fn :: [String] -> Int 
fn = go 0 0
  where 
    go current max [] = max
    go current max ("":ys) = go 0 (if current > max then current else max) ys
    go current max (y:ys) = go (current + read y) max ys
