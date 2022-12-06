{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  (print . fn) input

fn :: [Char] -> Int
fn = go 0
  where
    messageWidth = 14
    go idx [] = idx
    go idx xs | S.size (S.fromList (take messageWidth xs)) == messageWidth = idx + messageWidth
    go idx (_x1 : xs) = go (idx + 1) xs
