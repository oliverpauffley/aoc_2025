module Main where
import Data.Foldable (maximumBy)
import Data.Array

main :: IO ()
main = do
    file <- readFile "day3/input/day_3.txt"
    putStrLn "Part 1: "
    print $ part1 file
    putStrLn "Part 2: "
    print $ part2 file

part1 :: String -> Int
part1 = sum . map joltage . lines

-- given a string, find the maximum number that can be produced by taking two numbers in order from that string.
joltage :: String -> Int
joltage xs = maximum $ go xs
  where
    go :: String -> [Int]
    go [] = [0]
    go [x] = [read [x]]
    go (x:xs) = read [x , maximumBy (charIntComp) xs] : go xs

charIntComp :: Char -> Char -> Ordering
charIntComp a b = compare (r [a]) (r [b])
  where r :: String -> Int
        r = read

part2 :: String -> Int
part2 xs = sum $ map (\s -> joltage' 12 $ map readChar s) $ lines xs

readChar :: Char -> Int
readChar c = read [c]

joltage' :: Int -> [Int] -> Int
joltage' size xs =
                let candidates = [ memo ! (x,size) | x <- [0..l-1]]
                in intJoin $ maximumBy (\a b -> compare (intJoin a ) (intJoin b)) candidates
  where
    l = length xs
    s = listArray (0, l-1) xs

    memo :: Array (Int, Int) [Int]
    memo = tabulate ((0,0), (l-1, size)) solve

    solve (idx, 1) = [s ! idx]
    solve (idx, len)
      | l - idx < len = []
      | l - idx == len =  map (s !) [idx..l-1]
      | otherwise =
        let x = s ! idx
            candidates = [ memo ! (i,len-1) | i <- [idx+1..l-1] ]
        in x : maximumBy (\a b -> compare (intJoin a) (intJoin b)) candidates

intJoin :: [Int] -> Int
intJoin = foldl addDigit 0
   where addDigit num d = 10*num + d


tabulate :: (Ix i) => (i, i) -> (i -> a) -> Array i a
tabulate rng f = listArray rng (map f $ range rng)


testInput = "987654321111111\n\
  \811111111111119\n\
  \234234234234278\n\
  \818181911112111"


-- >>> part1Test
-- 357
part1Test = part1 testInput

-- >>> part2Test
-- 1181577568619
part2Test = part2 testInput
