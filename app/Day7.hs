{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day7 (main) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Foldable
import Data.Ord
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

data FileType = File !Int | Folder !FsMap
  deriving Show

newtype FsMap = FsMap (Map T.Text FileType)
  deriving newtype (Semigroup, Monoid, Show)

data Env = Env
  { pwd :: ![T.Text]
  , fs_map :: !FsMap
  }
  deriving Show

init_env :: Env
init_env = Env [] (FsMap mempty)

push_path_to_env :: Int -> T.Text -> Env -> Env
push_path_to_env size file env = env { fs_map = fs_map' }
  where
    fs_map' = push_path_to_fs_map size file (pwd env) (fs_map env)

push_path_to_fs_map :: Int -> T.Text -> [T.Text] -> FsMap -> FsMap
push_path_to_fs_map size file = go . reverse
  where
    go [] (FsMap m) = FsMap $ Map.alter (\_ -> Just (File size)) file m
    go (p:ps) (FsMap m) = FsMap $ Map.alter (Just . update') p m
      where
        update' Nothing = Folder $ go ps mempty
        update' (Just (File _)) = File size
        update' (Just (Folder sub_fs_map)) = Folder $ go ps sub_fs_map

apply_action' :: T.Text -> Env -> Env
apply_action' buf env = case T.words buf of
  ["$", "cd", "/"]  -> env { pwd=[] }
  ["$", "cd", ".."] -> env { pwd=drop 1 (pwd env) }
  ["$", "cd", to]   -> env { pwd = to:pwd env }
  ("$":_)           -> env
  ("dir":_)         -> env
  [size, name]      -> push_path_to_env (read $ T.unpack size) name env
  _                 -> env

count_dirs :: FsMap -> (Int, [(T.Text, Int)])
count_dirs (FsMap m) = (total_size, dirs)
  where
    (Monoid.Sum total_size, dirs) = foldMap' go $ Map.toList m

    go (name, File size)        = (Monoid.Sum size, [])
    go (name, Folder sub_fs_map) = (Monoid.Sum size, (name, size):dirs)
      where
        (size, dirs) = count_dirs sub_fs_map

parse_final_fs_map :: T.Text -> FsMap
parse_final_fs_map = fs_map . foldl' (flip apply_action') init_env . T.lines
  
solve1 :: FsMap -> Int
solve1 = sum . filter (<= 100000) . fmap snd . snd . count_dirs

solve2 :: FsMap -> Maybe Int
solve2 fs_map = listToMaybe . filter (>= required_size) . sort . map snd $ dirs
  where
    required_size = max (30000000 - (70000000 - used_size)) 0
    (used_size, dirs) = count_dirs fs_map

main :: IO ()
main = T.interact $ T.pack . show . (solve1 &&& solve2) . parse_final_fs_map
