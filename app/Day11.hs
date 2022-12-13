{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}

module Day11 (main) where

import Data.Ord (Down(..))
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow ((&&&))
import qualified Control.Foldl as F
import Data.Maybe (fromMaybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Bifunctor
import Optics

data Operator = Inc | Mul
data Operand = Me | Num !Int

data Monkey = Monkey
  { _mItems :: [Int]
  , _mInspectCount :: !Int
  , _mOperation :: !(Operator, Operand)
  , _mTester :: !(Int, Int, Int)
  }

type Divider = Int -> Int

makeLenses ''Monkey

parse_monkey :: [T.Text] -> (Int, Monkey)
parse_monkey [id_buf, item_buf, op_buf, div_buf, t_buf, f_buf] = (mid, Monkey items 0 op tester)
  where
    mid = read . T.unpack . T.init . (!! 1) . T.words $ id_buf
    items = fmap (read . T.unpack) . T.splitOn ", " . (!! 1) . T.splitOn ": " $ item_buf
    op = case T.words . (!! 1) . T.splitOn "new = old " $ op_buf of
      ["*", "old"] -> (Mul, Me)
      ["+", "old"] -> (Inc, Me)
      ["*", n] -> (Mul, Num $ read $ T.unpack n)
      ["+", n] -> (Inc, Num $ read $ T.unpack n)
      _ -> error $ "oh no" <> T.unpack op_buf
    tester = (div', t', f')
    div' = read . T.unpack . (!! 1) . T.splitOn "by " $ div_buf
    t' = read . T.unpack . (!! 1) . T.splitOn "monkey " $ t_buf
    f' = read . T.unpack . (!! 1) . T.splitOn "monkey " $ f_buf
parse_monkey _ = error "oh no"

parse_monkeys :: T.Text -> IntMap Monkey
parse_monkeys = IM.fromList . fmap parse_monkey . chunksOf 6 . filter (not . T.null) . T.lines

run_op :: (Operator, Operand) -> Int -> Int
run_op (Inc, Me) = (* 2)
run_op (Inc, Num n) = (+ n)
run_op (Mul, Me) = (^ 2)
run_op (Mul, Num n) = (* n)

monkey_one_turn :: Divider -> Int -> IntMap Monkey -> IntMap Monkey
monkey_one_turn divider mid ms = ms
  & (\ms' -> foldl' (&) ms' $ fmap run items)
  & (ix mid % mItems .~ [])
  & (ix mid % mInspectCount %~ (+ length items))
  where
    Monkey items inspect_count op (div', t, f) = ms IM.! mid

    run :: Int -> IntMap Monkey -> IntMap Monkey
    run old_item ms' =
      let
        new_item = divider $ run_op op old_item
        target_id = if new_item `mod` div' == 0
          then t
	  else f
      in ms' & ix target_id % mItems %~ (<> [new_item])

monkey_one_round :: Divider -> IntMap Monkey -> IntMap Monkey
monkey_one_round divider ms = foldl' (&) ms $ fmap (monkey_one_turn divider) $ IM.keys ms

monkey_rounds :: Divider -> Int -> IntMap Monkey -> IntMap Monkey
monkey_rounds divider round_num ms =
  foldl' (&) ms $ replicate round_num $ monkey_one_round divider

monkey_solve :: Divider -> Int -> IntMap Monkey -> Int
monkey_solve divider round_num = product . take 2 . sortOn Down . fmap _mInspectCount . IM.elems . monkey_rounds divider round_num

dividerX :: IntMap Monkey -> Int -> Int
dividerX ms = (`mod` the_num)
  where
    !the_num = product $ fmap (view _1 . _mTester) $ IM.elems ms

solve1 :: IntMap Monkey -> Int
solve1 = monkey_solve (`div` 3) 20

solve2 :: IntMap Monkey -> Int
solve2 ms = monkey_solve (dividerX ms) 10000 ms

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2) . parse_monkeys
