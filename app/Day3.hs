module Day3 (main) where

import qualified Data.Text as TL
import qualified Data.Text.IO as TL

import Data.List.Split
import Data.Foldable

import Control.Arrow
import Control.DeepSeq

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

sumSetBy :: Num b => (a -> b) -> Set a -> b
sumSetBy f = Set.foldl' (\acc e -> acc + f e) 0

splitAndFindDup :: TL.Text -> Set Char
splitAndFindDup ln = lset `Set.intersection` rset
  where
    lset = Set.fromList $ TL.unpack l
    rset = Set.fromList $ TL.unpack r
    (l, r) = TL.splitAt mid ln
    mid = TL.length ln `div` 2

score :: Char -> Int
score c
  | isLower c = (fromEnum c) - (fromEnum 'a') + 1
  | isUpper c = (fromEnum c) - (fromEnum 'A') + 27
  | otherwise  = 0

findBadge :: (Foldable t, Functor t) => t TL.Text -> Set Char
findBadge xs
  | null xs = mempty
  | otherwise = foldl1 Set.intersection . fmap (Set.fromList . TL.unpack) $ xs

solve1 :: TL.Text -> Int
solve1 = sum . fmap (sumSetBy score . splitAndFindDup) . TL.lines

solve2 :: TL.Text -> Int
solve2 = sum . fmap (sumSetBy score . findBadge) . chunksOf 3 . TL.lines

main :: IO ()
main = TL.interact (TL.pack . show . (solve1 &&& solve2))
