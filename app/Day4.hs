{-# LANGUAGE OverloadedStrings #-}
module Day4 (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (al, ar) (bl, br) = (al <= bl && br <= ar) || (bl <= al && ar <= br)

has_overlap :: (Int, Int) -> (Int, Int) -> Bool
has_overlap (al, ar) (bl, br) = actual_length > max_length
  where
    max_length = r - l + 1
    actual_length = a_length + b_length
    a_length = ar - al + 1
    b_length = br - bl + 1
    l = min al bl
    r = max ar br

solve_by :: ((Int, Int) -> (Int, Int) -> Bool) -> T.Text -> Int
solve_by pred = length . filter id . fmap (uncurry pred . parse) . T.lines
  where
    parse :: T.Text -> ((Int, Int), (Int, Int))
    parse s =
      let [l, r] = T.splitOn "," s
      in (parseRange l, parseRange r)

    parseRange :: T.Text -> (Int, Int)
    parseRange s = let [begin, end] = T.splitOn "-" s
      in (parseDigit begin, parseDigit end)

    parseDigit :: T.Text -> Int
    parseDigit = read . T.unpack 

solve1 :: T.Text -> Int
solve1 = solve_by fullyContains

solve2 :: T.Text -> Int
solve2 = solve_by has_overlap

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2)
