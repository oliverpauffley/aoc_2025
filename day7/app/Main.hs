{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntSet as S
import Data.Foldable (foldl')
import Data.List (tails, elemIndex)
import GHC.Arr (listArray, (!), Ix (..))
import Data.MemoTrie

main :: IO ()
main = do
    file <-  TIO.readFile "day7/input/day_7.txt"
    putStrLn "Part 1: "
    print $ part1 $  file
    putStrLn "Part 2: "
    print $ part2 file


part1 :: T.Text -> Int
part1 xs = solve (T.lines xs) S.empty


solve :: [T.Text] -> S.IntSet -> Int
solve [] _ = error "empty list"
solve (x:xs) set = go xs ((S.insert (findStart x) set), 0)
  where
        go :: [T.Text] -> (S.IntSet, Int) -> Int
        go [] (_, acc) = acc
        go (l:ls) (s, acc) = go ls $ foldl' (split l) (s,acc) (S.elems s)

        (min, max) = (0, T.length x -1)

        split :: T.Text -> (S.IntSet, Int) -> Int -> (S.IntSet, Int)
        split l u@(s,acc) t = case T.index l t of
          '.' -> u
          '^' -> let
                   s' = S.delete t s
                   below = t - 1
                   above = t + 1
                   (s'',acc') = if below >=min then (S.insert below s',acc+1) else (s',acc)
                   in if above <= max then (S.insert above s'',acc+1) else (s'',acc')





-- return the position of the start
findStart :: T.Text ->  Int
findStart xs = case T.findIndex (=='S') xs of
  Nothing -> error "no start"
  Just s -> s

-- part2 :: T.Text -> Int
-- part2 xs = count $ go (tail $ T.lines xs) start (State S.empty 1)
--   where start = findStart (head $ T.lines xs)
--         go :: [T.Text] -> Index -> State -> State
--         go [] _ state = state
--         go (l:ls) t state = case T.index l t of
--           '.' -> go ls t state
--           '^' -> case (inState state (t-1), inState state (t+1)) of
--             (True, True) -> state
--             (True, False) -> go ls (t-1) (updateState state t (t-1))
--             (False, True) -> go ls (t+1) (updateState state t(t+1))
--             (_, _) -> go ls (t-1) (updateState state t (t-1)) `mergeState` go ls (t+1) (updateState state t (t+1))


part2 :: T.Text -> Int
part2 xs = go (findStart (head $ T.lines xs), 0)
  where
    lines = tail $ T.lines xs
    max = length lines - 1
    arr = listArray (0, max) lines
    go = memo memoSolve
    memoSolve (_, n) | n==max  = 1
    memoSolve (idx, lidx) = case T.index (arr ! lidx) idx of
      '.' -> go (idx, lidx + 1)
      '^' -> go (idx -1, lidx+1) + go (idx +1, lidx + 1)
      _otherwise -> error "unknown char"




testCase :: T.Text
testCase =
  ".......S.......\n\
\...............\n\
\.......^.......\n\
\...............\n\
\......^.^......\n\
\...............\n\
\.....^.^.^.....\n\
\...............\n\
\....^.^...^....\n\
\...............\n\
\...^.^...^.^...\n\
\...............\n\
\..^...^.....^..\n\
\...............\n\
\.^.^.^.^.^...^.\n\
\..............."

-- >>> testPart1
-- 21
testPart1 = part1 testCase

-- >>> testPart2
-- 40
testPart2 = part2 testCase
