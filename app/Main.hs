{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Arrow ((***))
import Control.DeepSeq (deepseq)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class 
import Control.Monad.Reader (runReaderT)
import Control.Monad
import Control.Concurrent.Async
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Function
import Data.Word
import Data.IORef
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Streamly.Prelude ((|&), (|&.))
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Console.Stdio as Stdio
import Streamly.Data.Fold.Tee (Tee (Tee))
import qualified Streamly.Data.Fold.Tee as Tee
import qualified Streamly.Unicode.Stream as Uni
import qualified Streamly.Internal.Unicode.Array.Char as Uni
import qualified Streamly.Internal.FileSystem.FD as FD
import qualified Streamly.Internal.Data.Array.Foreign.Type as Arr
import Control.Parallel.Strategies

import Data.Proxy

import Data.Foldable
import Options.Applicative

import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day7 as Day7
import qualified Day8 as Day8
import qualified Day9 as Day9
import qualified Day10 as Day10

opt_parser = info (helper <*> cmds)
  ( fullDesc
  )
  where
    cmds = hsubparser
      (  command "day3" (info (pure Day3.main) idm)
      <> command "day4" (info (pure Day4.main) idm)
      <> command "day5" (info (pure Day5.main) idm)
      <> command "day6" (info (pure Day6.main) idm)
      <> command "day7" (info (pure Day7.main) idm)
      <> command "day9" (info (pure Day9.main) idm)
      <> command "day10" (info (pure Day10.main) idm)
      )

main :: IO ()
main = execParser opt_parser >>= id

solveS :: T.Text -> Int
solveS = uncurry finalScore . parseShape

solveS' :: T.Text -> Int
solveS' = uncurry finalScore . uncurry computeResult . parseShapeAndResult
  where
    computeResult :: Shape -> Ordering -> (Shape, Shape)
    computeResult s ord = (s, computeYourMove s ord)

main' :: IO ()
main' = do
  ret <- 
    ( FD.read FD.stdin
    & Uni.decodeLatin1
    & Uni.lines
    & fmap
      ( T.pack
      . Arr.toList
      )
    & fmap ((,) <$> solveS <*> solveS')
    & Stream.fold (Fold.unzip Fold.sum Fold.sum)
    )

  print $ fst ret
  print $ snd ret

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

instance Ord Shape where
  compare Rock Scissors = GT
  compare Scissors Paper = GT
  compare Paper Rock = GT
  compare l r
    | l == r = EQ
    | otherwise = LT

abcToShape :: T.Text -> Shape
abcToShape "A" = Rock
abcToShape "B" = Paper
abcToShape "C" = Scissors
abcToShape c = error $ "invalid input to convert to Shape: " <> show c

xyzToShape :: T.Text -> Shape
xyzToShape "X" = Rock
xyzToShape "Y" = Paper
xyzToShape "Z" = Scissors
xyzToShape c = error $ "invalid input to convert to Shape: " <> show c

xyzToResult :: T.Text -> Ordering
xyzToResult "X" = GT
xyzToResult "Y" = EQ
xyzToResult "Z" = LT
xyzToResult c = error $ "invalid input to convert to game result: " <> show c

parseShape :: T.Text -> (Shape, Shape)
parseShape t = let [l, r] = T.words t in (abcToShape l, xyzToShape r)

parseShapeAndResult :: T.Text -> (Shape, Ordering)
parseShapeAndResult t = let [l, r] = T.words t in (abcToShape l, xyzToResult r)

moveToWin :: Shape -> Shape
moveToWin Rock = Paper
moveToWin Paper = Scissors
moveToWin Scissors = Rock

computeYourMove :: Shape -> Ordering -> Shape
computeYourMove s EQ = s
computeYourMove s LT = moveToWin s
computeYourMove s GT = moveToWin (moveToWin s)

winScore :: Shape -> Shape -> Int
winScore l r = case compare l r of
  LT -> 6
  EQ -> 3
  GT -> 0

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

finalScore :: Shape -> Shape -> Int
finalScore l r = shapeScore r + winScore l r
