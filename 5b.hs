{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.Char (isLetter)
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  ( print
      . (concatMap (head . snd))
      . M.toList
      . uncurry applyAll
      . bimap
        (toState . map reverse . transpose . map (chunksOf 4))
        (map (unsafeTriplets . words))
      . splitAt 9
      . filter (/= "")
      . lines
    )
    input

toState :: [[String]] -> M.Map Int [String]
toState xs = go M.empty xs
  where
    go m [] = m
    go m ((x : xs) : ys) =
      go
        ( M.insert
            (read $ trim x)
            (reverse $ filter (/= "") $ map (filter isLetter) xs)
            m
        )
        ys

unsafeTriplets :: [String] -> (Int, Int, Int)
unsafeTriplets [_, x, _, y, _, z] = (read x, read y, read z)

apply :: M.Map Int [String] -> (Int, Int, Int) -> M.Map Int [String]
apply state (amount, from, to) = insertion
  where
    extraction = fromJust $ take amount <$> M.lookup from state
    deletion = M.updateWithKey (\k x -> Just (drop amount x)) from state
    insertion = M.updateWithKey (\k x -> Just (extraction ++ x)) to deletion

applyAll :: M.Map Int [String] -> [(Int, Int, Int)] -> M.Map Int [String]
applyAll = foldl' apply
