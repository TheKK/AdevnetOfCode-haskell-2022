{-# LANGUAGE OverloadedStrings #-}

module Day6 (main) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

-- Idea of transpose' from:
-- https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

window :: Int -> [a] -> [[a]]
window m = transpose' . take m . tails

unique_with_length :: Int -> T.Text -> Int
unique_with_length n = fromMaybe 0 . fmap (+ n) . findIndex is_unique . window n . T.unpack
  where
    is_unique s = nub s == s

solve1 :: T.Text -> Int
solve1 = unique_with_length 4

solve2 :: T.Text -> Int
solve2 = unique_with_length 14

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2)
