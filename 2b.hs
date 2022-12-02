{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Tuple
import Lib
import Prelude hiding (sequence)

main :: IO ()
main = do
  input <- getContents
  ( print
      . sum
      . map (\xs -> outcomeToInt (snd xs) + playToInt (chooseCounter xs))
      . mapMaybe (sequence . bimap toPlay toOutcome)
      . mapMaybe (tuples . words)
      . lines
    )
    input

tuples :: [a] -> Maybe (a, a)
tuples [x, y] = Just (x, y)
tuples _ = Nothing

sequence :: (Maybe a, Maybe b) -> Maybe (a, b)
sequence (Just a, Just b) = Just (a, b)
sequence _ = Nothing

data Play = Rock | Paper | Scissors deriving (Show, Eq)

data Outcome = Loss | Win | Draw deriving (Show)

toOutcome "X" = Just Loss
toOutcome "Y" = Just Draw
toOutcome "Z" = Just Win
toOutcome _ = Nothing

toPlay "A" = Just Rock
toPlay "B" = Just Paper
toPlay "C" = Just Scissors
toPlay _ = Nothing

playToInt :: Play -> Int
playToInt Rock = 1
playToInt Paper = 2
playToInt Scissors = 3

chooseCounter :: (Play, Outcome) -> Play
chooseCounter (Rock, Win) = Paper
chooseCounter (Rock, Loss) = Scissors
chooseCounter (Paper, Win) = Scissors
chooseCounter (Paper, Loss) = Rock
chooseCounter (Scissors, Win) = Rock
chooseCounter (Scissors, Loss) = Paper
chooseCounter (x, Draw) = x

outcomeToInt :: Outcome -> Int
outcomeToInt Loss = 0
outcomeToInt Draw = 3
outcomeToInt Win = 6
