{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Day8 (main) where

import Data.Semigroup
import Control.Arrow
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Array as UA

type Map = UA.Array (Int, Int) Int

raw_map :: T.Text -> Map
raw_map buf = UA.listArray ((0, 0), (rows - 1, cols - 1))
  $ map ((\n -> n - fromEnum '0') . fromEnum)
  $ concatMap T.unpack lines'
  where
    cols = T.length $ head lines'
    rows = length lines'
    lines' = T.lines buf

on_edge :: Map -> (Int, Int) -> Bool
on_edge m = not . UA.inRange ((br + 1, bc + 1), (er - 1, ec - 1))
  where
   ((br, bc), (er, ec)) = UA.bounds m

visible_in_row_n_col :: Map -> (Int, Int) -> Bool
visible_in_row_n_col m pos@(r, c) = any id [l_visible, r_visible, u_visible, b_visible]
  where
    l_visible = all (< tree_height) [m UA.! (r, c') | c' <- [bc..(c - 1)]]
    r_visible = all (< tree_height) [m UA.! (r, c') | c' <- [(c + 1)..ec]]
    u_visible = all (< tree_height) [m UA.! (r', c) | r' <- [br..(r - 1)]]
    b_visible = all (< tree_height) [m UA.! (r', c) | r' <- [(r + 1)..er]]
    tree_height = m UA.! pos
    ((br, bc), (er, ec)) = UA.bounds m

tree :: Int -> [Int] -> Int
tree h = go 0
  where
    go acc []     = acc
    go acc (t:ts) = if h > t then go (acc + 1) ts else acc + 1

tree_in_row_n_col :: Map -> (Int, Int) -> Int
tree_in_row_n_col m pos@(r, c) = product [l_visible, r_visible, u_visible, b_visible]
  where
    l_visible = tree tree_height $ [m UA.! (r, c') | c' <- reverse [bc..(c - 1)]]
    r_visible = tree tree_height $ [m UA.! (r, c') | c' <- [(c + 1)..ec]]
    u_visible = tree tree_height $ [m UA.! (r', c) | r' <- reverse [br..(r - 1)]]
    b_visible = tree tree_height $ [m UA.! (r', c) | r' <- [(r + 1)..er]]
    tree_height = m UA.! pos
    ((br, bc), (er, ec)) = UA.bounds m

solve1 :: Map -> Int
solve1 m = length . filter id . fmap (visible_in_row_n_col m) . filter (not . (on_edge m)) $ UA.indices $ m

solve2 :: Map -> Int
solve2 m = maximum $ fmap (tree_in_row_n_col m) $ filter (not . (on_edge m)) $ UA.indices m

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2) . raw_map
