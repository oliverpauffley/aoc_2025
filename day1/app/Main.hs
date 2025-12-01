module Main where

main :: IO ()
main = do
    file <- readFile "day1/input/day_1.txt"
    putStrLn "Part 1: "
    print $ part1 file
    putStrLn "Part 2: "
    print $ part2 file

data Direction = L Int | R Int
    deriving (Show)

move :: Int -> Direction -> Int
move v (L x) = (v - x) `mod` 100
move v (R x) = (v + x) `mod` 100

parse :: String -> Direction
parse ('L' : num) = L $ read num
parse ('R' : num) = R $ read num
parse _ = error "can't parse"

part1 :: String -> Int
part1 input = go 0 50 (lines input)
  where
    go count _ [] = count
    go count cur (x : xs) = case move cur (parse x) of
        0 -> go (count + 1) 0 xs
        cur' -> go count cur' xs

part2 :: String -> Int
part2 input = go 0 50 (lines input)
  where
    go count _ [] = count
    go count cur (x : xs) = case move' cur (parse x) of
        (0, c) -> go (count + c) 0 xs
        (cur', c) -> go (count + c) cur' xs

type Count = Int
type Pointing = Int

move' :: Int -> Direction -> (Pointing, Count)
move' val d = case d of
    (R x) -> go val (val + x, 0)
    (L x) -> go val (val - x, 0)
  where
    go pos (v, c)
        | pos == 0 && v < 0 = go (100 + v) (100 + v, c)
        | v < 0 = go (v + 100) (v + 100, c + 1)
        | v > 99 = go (v - 100) (v - 100, c + 1)
        | otherwise = (v, c)
