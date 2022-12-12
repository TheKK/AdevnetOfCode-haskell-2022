{-# LANGUAGE OverloadedStrings #-}

module Day5 (main) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as T

update' :: (a -> a) -> IM.Key -> IntMap a -> IntMap a
update' f k m = IM.update (Just . f) k m

parse_single_stack :: T.Text -> (Int, [Char])
parse_single_stack input = (read $ pure $ T.head input, T.unpack $ T.reverse $ T.tail input)

parse_init_stack :: T.Text -> IntMap [Char]
parse_init_stack = IM.fromList
  . fmap (parse_single_stack)
  . filter (not . T.null)
  . map (T.reverse . T.strip . T.filter (\c -> not $ c `elem` ("[]" :: [Char])))
  . T.transpose
  . T.lines

data Move = Move Int Int Int

parse_action :: T.Text -> Move
parse_action input = Move count from to
  where
    (count, from, to) = (read count', read from', read to')
    [_, count', _, from', _, to'] = T.unpack <$> T.words input

crate_mover_9000 :: Move -> IntMap [Char] -> IntMap [Char]
crate_mover_9000 (Move count from to) m = updates m
  where
    updates m' = foldl' (&) m' $ replicate count $ apply_single_move from to

    apply_single_move :: Int -> Int -> IntMap [Char] -> IntMap [Char]
    apply_single_move from to m =
      let item = head $ m IM.! from
      in m
        & update' (drop 1) from
        & update' (item:) to

crate_mover_9001 :: Move -> IntMap [Char] -> IntMap [Char]
crate_mover_9001 (Move count from to) m = apply_single_move m
  where
    apply_single_move :: IntMap [Char] -> IntMap [Char]
    apply_single_move m =
      let crates_to_move = take count $ m IM.! from
      in m
        & update' (drop count)        from
        & update' (crates_to_move <>) to

extract_result :: IntMap [Char] -> T.Text
extract_result = T.pack . concatMap (take 1 . snd) . IM.toList

solve_with :: (Move -> IntMap [Char] -> IntMap [Char]) -> T.Text -> T.Text
solve_with mover input = extract_result $ runable_actions init_stack
  where
    runable_actions stack = foldl' (&) stack $ map mover actions
    init_stack = parse_init_stack raw_init_stack
    actions = map parse_action $ T.lines raw_actions
    [raw_init_stack, raw_actions] = T.splitOn "\n\n" input

main :: IO ()
main = T.interact
  $ T.pack . show . (solve_with crate_mover_9000 &&& solve_with crate_mover_9001)
