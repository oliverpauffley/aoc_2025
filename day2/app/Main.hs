module Main where

import Data.List.Split

main :: IO ()
main = do
    file <- readFile "day2/input/day_2.txt"
    putStrLn "Part 1: "
    print $ part1 file
    putStrLn "Part 2: "
    print $ part2 file

part1 :: String -> Int
part1 = sum . map (solve . toRange) . splitOn ","

toRange :: String -> [Int]
toRange xs = [(read start) .. (read end)]
  where [start,end] = splitOn "-" xs

solve :: [Int] -> Int
solve xs = sum $ map go xs
  where
    go :: Int -> Int
    go x
         | odd l = 0
         | uncurry (==) $ splitAt (l `div` 2) (show x) = x
         | otherwise = 0
         where l = length (show x)


-- >>> part1Test
-- 1227775554
part1Test :: Int
part1Test = part1 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

part2 :: String -> Int
part2 = sum . map (solve' . toRange) . splitOn ","

solve' :: [Int] -> Int
solve' xs = sum $ map go xs
  where
    go :: Int -> Int
    go x
         | any allEq $ [ chunksOf n (show x) | n <- [1..(l `div` 2)]] = x
         | otherwise = 0
         where l = length (show x)

allEq :: Eq a => [a] -> Bool
allEq [] = False
allEq [_] = True
allEq [x,y] = x == y
allEq (x:y:xs) = x == y && allEq (y:xs)

-- >>> part2Test
part2Test :: Int
part2Test = part2 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
