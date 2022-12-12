{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Day9 (main) where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow

data Pos = Pos
  { posX :: {-# UNPACK #-} !Int
  , posY :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord)

data Direction = U | D | L | R
  deriving (Read)

move_to :: Direction -> Pos -> Pos
move_to U (Pos x y) = Pos x (y + 1)
move_to D (Pos x y) = Pos x (y - 1)
move_to L (Pos x y) = Pos (x - 1) y
move_to R (Pos x y) = Pos (x + 1) y

pos_distance :: Pos -> Pos -> Double
pos_distance (Pos x y) (Pos x' y') = sqrt $
   fromIntegral $ (x - x')^(2 :: Int) + (y - y')^(2 :: Int)

-- | 'pulling_knot_toward a b' make a moving.
pulling_knot_toward :: Pos -> Pos -> Pos
pulling_knot_toward src@(Pos sx sy) to@(Pos tx ty)
  | pos_distance src to <= sqrt 2 = src
  | otherwise = case dx `compare` dy of
    LT -> Pos tx y'
    EQ -> Pos x' y'
    GT -> Pos x' ty
  where
    dx = abs $ sx - tx
    dy = abs $ sy - ty
    x' = if tx > sx then tx - 1 else tx + 1
    y' = if ty > sy then ty - 1 else ty + 1

pulling_rope :: Direction -> NonEmpty Pos -> NonEmpty Pos
pulling_rope dir (old_head_pos :| old_bodies) = new_rope
  where
    new_rope = NE.scanl1
      (\to body -> pulling_knot_toward body to)
      (new_head_pos :| old_bodies)
    new_head_pos = move_to dir old_head_pos

data World = World
  { world_rope :: !(NonEmpty Pos)
  , world_walked_path :: !(Set Pos)
  }

-- | rope consist with 1 head and n body/bodies.
init_world_with_body_of :: Int -> World
init_world_with_body_of body_num = World
  (Pos 0 0 :| replicate body_num (Pos 0 0))
  (Set.singleton $ Pos 0 0)

one_step_world :: Direction -> World -> World
one_step_world dir w = w
  { world_rope = new_rope
  , world_walked_path = Set.insert (NE.last new_rope) (world_walked_path w)
  }
  where
    new_rope = pulling_rope dir $ world_rope w

step_world :: (Direction, Int) -> World -> World
step_world (dir, distance) w = foldl' (&) w $ replicate distance $ one_step_world dir

run_world :: [(Direction, Int)] -> World -> World
run_world cmds w = foldl' (&) w $ fmap step_world cmds

parse_cmds :: T.Text -> [(Direction, Int)]
parse_cmds = fmap parse_cmd . T.lines
  where
    parse_cmd buf =
      let [dir, distance] = T.words buf
      in (read $ T.unpack dir, read $ T.unpack distance)

solve_with_body_count :: Int -> [(Direction, Int)] -> Int
solve_with_body_count n cmds = length $ world_walked_path $ run_world cmds $ init_world_with_body_of n

solve1, solve2 :: [(Direction, Int)] -> Int
solve1 = solve_with_body_count 1
solve2 = solve_with_body_count 9

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2) . parse_cmds
