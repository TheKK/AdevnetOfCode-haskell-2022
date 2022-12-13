{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Day12 (main) where

import GHC.Generics
import Control.Parallel.Strategies
import Data.Ord (Down(..))
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow ((&&&))
import qualified Control.Foldl as F
import Data.Maybe (fromMaybe, fromJust, maybeToList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Array.Unboxed as UA
import Optics

bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOn rep gen init_state = loop Set.empty init_state
  where
    loop seen = \case
      [] -> []
      (x:xs) -> if rep_x `elem` seen
        then loop seen xs
        else x : loop (Set.insert rep_x seen) (xs <> gen x)
        where
          rep_x = rep x

data Pos = Pos !Int !Int
  deriving (Eq, Ord, Show)

instance UA.Ix Pos where
  range ((Pos x y), (Pos x' y')) = map (\(a, b) -> Pos a b) $ UA.range ((x, y), (x', y'))
  index ((Pos x y), (Pos x' y')) (Pos a b) = UA.index ((x, y), (x', y')) (a, b)
  inRange ((Pos x y), (Pos x' y')) (Pos a b) = UA.inRange ((x, y), (x', y')) (a, b)

data World = World
  { _wMap :: UA.Array Pos Char
  , _wMe :: !Pos
  , _wStep :: !Int
  , _wTarget :: !Pos
  }

makeLenses ''World

height_at :: Pos -> World -> Int
height_at pos w = fromEnum (_wMap w UA.! pos)

parse_world :: T.Text -> World
parse_world buf = World wmap wme 0 wtarget
  where
    wmap = init_wmap UA.//
      [ (wme, 'a')
      , (wtarget, 'z')
      ]
      
    init_wmap = UA.listArray (Pos 0 0, Pos (world_col - 1) (world_row - 1))
      $ T.unpack
      $ T.filter (/= '\n')
      $ buf

    wme = fst . fromJust . find ((== 'S') . snd) . UA.assocs $ init_wmap
    wtarget = fst . fromJust . find ((== 'E') . snd) . UA.assocs $ init_wmap

    world_row = T.length $ head $ bufs
    world_col = length bufs
    bufs = T.lines buf

is_goal :: World -> Bool
is_goal w = _wMe w == _wTarget w

data Dir = U | D | L | R

move_pos :: Dir -> Pos -> Pos
move_pos U (Pos c r) = Pos (c - 1) r
move_pos D (Pos c r) = Pos (c + 1) r
move_pos L (Pos c r) = Pos c (r - 1)
move_pos R (Pos c r) = Pos c (r + 1)

walk_world_toward :: Dir -> World -> Maybe World
walk_world_toward dir w = if moveable
  then Just $ w
    & wMe .~ new_pos
    & wStep .~ new_step
  else Nothing

  where
    moveable
      = UA.inRange (UA.bounds (_wMap w)) new_pos
      &&
        (  (new_height >= current_height && new_height - current_height <= 1)
        || (new_height < current_height)
        )

    new_step = _wStep w + 1

    new_pos = move_pos dir $ _wMe w
    new_height = height_at new_pos w

    current_pos = _wMe w
    current_height = height_at current_pos w

walk_world_once :: World -> [World]
walk_world_once w = concat $ fmap (\d -> maybeToList $ walk_world_toward d w) [U, D, L, R]

walk_world_until_goal :: [World] -> [World]
walk_world_until_goal ws = filter is_goal $ bfsOn _wMe walk_world_once ws

debug_world :: World -> T.Text
debug_world w = T.unlines $ do
  c <- [0..c_max] 

  pure $ T.pack $ do
    r <- [0..r_max]
    pure $ _wMap w UA.! (Pos c r)

  where
    (_, Pos c_max r_max) = UA.bounds $ _wMap w

all_a_world :: World -> [World]
all_a_world w = [ w { _wMe = pos } | (pos, c) <- UA.assocs $ _wMap w, c == 'a']

solve1 = minimum . map _wStep . walk_world_until_goal . singleton . parse_world
solve2 = minimum . map _wStep . walk_world_until_goal . all_a_world . parse_world

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2)
