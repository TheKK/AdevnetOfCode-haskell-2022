{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Day10 (main) where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow ((&&&))
import qualified Control.Foldl as F
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor

data Inst = Addx !Int | Noop
data TimedInst = TAddx !Int | TNoop

insts_to_timed :: [Inst] -> [TimedInst]
insts_to_timed = concatMap $ \ case
  (Addx n) -> [TNoop, TAddx n]
  Noop -> [TNoop]

data Env = Env { regX :: !Int, crt_pos :: !Int, drawn_pixels :: !(Set Int) }
  deriving (Show)

init_env :: Env
init_env = Env 1 0 Set.empty

step_env :: TimedInst -> Env -> Env
step_env inst env = env { crt_pos = 1 + crt_pos env, regX = regX', drawn_pixels = drawn_pixels' }
  where
    regX' = case inst of
      (TAddx n) -> n + (regX env)
      TNoop     -> regX env
    drawn_pixels' = if crt_horizon_pos `elem` sprit_pixels
      then Set.insert (crt_pos env) (drawn_pixels env)
      else drawn_pixels env

    crt_horizon_pos = crt_pos env `mod` 40
    sprit_pixels = fmap ($ regX env) [pred, id, succ]

render_crt :: Env -> T.Text
render_crt env = T.unlines $ do
  y <- [0..5]
  pure $ T.pack $ do
    x <- [0..39]
    pure $ if (y * 40 + x) `elem` pixels then '#' else '.'
  where
    pixels = drawn_pixels env

parse_insts :: T.Text -> [Inst]
parse_insts = fmap parse_inst . T.lines
  where
    parse_inst buf = case T.words buf of
      ["addx", n] -> Addx $ read $ T.unpack n
      _           -> Noop

solve1 :: [TimedInst] -> Int
solve1 = F.fold result_fold . scanl' (flip step_env) init_env
  where
    result_fold = fmap sum
      $ sequenceA
      $ fmap (fmap (fromMaybe 0) . signal_strength_fold)
      $ [20, 60, 100, 140, 180, 220]

    signal_strength_fold :: Int -> F.Fold Env (Maybe Int)
    signal_strength_fold n = fmap (fmap $ \(Env reg _ _) -> n * reg) $ F.index (n -1)

solve2 :: [TimedInst] -> T.Text
solve2 = render_crt . foldl' (flip step_env) init_env

two_lines :: T.Text -> T.Text -> T.Text
two_lines a b = T.unlines [a, b]

main :: IO ()
main = T.interact $ uncurry two_lines . first (T.pack . show) . (solve1 &&& solve2) . insts_to_timed . parse_insts
