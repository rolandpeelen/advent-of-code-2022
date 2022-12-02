{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.List
import Data.Maybe
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  ( print
      . sum
      . map (\(a, b) -> outcomeToInt (score (a, b)) + playToInt b)
      . mapMaybe (tuples . map toPlay . words)
      . lines
    )
    input

tuples :: [Maybe a] -> Maybe (a, a)
tuples [Just x, Just y] = Just (x, y)
tuples _ = Nothing

data Play = Rock | Paper | Scissors deriving (Show, Eq)

data Outcome = Loss | Win | Draw deriving (Show)

toPlay "A" = Just Rock
toPlay "B" = Just Paper
toPlay "C" = Just Scissors
toPlay "X" = Just Rock
toPlay "Y" = Just Paper
toPlay "Z" = Just Scissors
toPlay _ = Nothing

playToInt :: Play -> Int
playToInt Rock = 1
playToInt Paper = 2
playToInt Scissors = 3

score :: (Play, Play) -> Outcome
score (Rock, Paper) = Win
score (Paper, Scissors) = Win
score (Scissors, Rock) = Win
score (x, y) | x == y = Draw
score _ = Loss

outcomeToInt :: Outcome -> Int
outcomeToInt Loss = 0
outcomeToInt Draw = 3
outcomeToInt Win = 6
