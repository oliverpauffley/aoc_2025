module Main where
import Data.Array
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    file <- readFile "day4/input/day_4.txt"
    putStrLn "Part 1: "
    print $ part1 file
    putStrLn "Part 2: "
    print $ part2 file

part1 :: String -> Int
part1 xs =
  let arr = buildArray xs
      arr' = assocs arr
      in solve arr arr' 0

solve :: Array (Int, Int) Char -> [((Int,Int), Char)] -> Int -> Int
solve _ [] acc = acc
solve arr ((_,'.'):xs) acc = solve arr xs acc -- skip non rolls
solve arr ((i,'@'):xs) acc = if isAccessible i then solve arr xs (acc+1) else solve arr xs acc
  where isAccessible i =
          let neighbours = catMaybes $ map (arr !?) (adjacentIdx i)
              accessible = (<4) $ length $ filter (=='@') $ neighbours
          in accessible

part2 :: String -> Int
part2 xs =
  let arr = buildArray xs
   in solvePart2 arr 0

solvePart2 arr count =
           let toRemove = solve' arr (assocs arr) []
           in case toRemove of
             [] -> count
             r -> solvePart2 (arr // [(i, '.') | i <- r]) (count + length r)

solve' :: Array (Int, Int) Char -> [((Int,Int), Char)] -> [(Int,Int)] -> [(Int,Int)]
solve' _ [] acc = acc
solve' arr ((_,'.'):xs) acc = solve' arr xs acc -- skip non rolls
solve' arr ((i,'@'):xs) acc = if isAccessible i then solve' arr xs (i:acc) else solve' arr xs acc
  where isAccessible i =
          let neighbours = catMaybes $ map (arr !?) (adjacentIdx i)
              accessible = (<4) $ length $ filter (=='@') $ neighbours
          in accessible


testCase =
   "..@.@@@@.\n\
   \@@@.@.@.@@\n\
   \@@@@@.@.@@\n\
   \@.@@@@..@.\n\
   \@@.@@@@.@@\n\
   \.@@@@@@@.@\n\
   \.@.@.@.@@@\n\
   \@.@@@.@@@@\n\
   \.@@@@@@@@.\n\
   \@.@.@@@.@.\n"


-- >>> testPart1
-- 19
testPart1 = part1 testCase


-- >>> testPart2
-- 43
testPart2 = part2 testCase

buildArray :: String -> Array( Int,Int) Char
buildArray xs = listArray ((0,0), (y-1,x-1)) [ a | ys <- ls, a <- ys ]
  where ls = lines xs
        x = length $ head ls
        y = length ls


-- >>> adjacentIdx (3,3)
-- [(2,2),(2,3),(2,4),(3,2),(3,4),(4,2),(4,3),(4,4)]
adjacentIdx :: (Int,Int) -> [(Int,Int)]
adjacentIdx (y, x) = [(y+y', x + x') | y' <- [-1,0, 1], x' <- [-1,0,1], x' /= 0 || y' /= 0 ]

-- >>> testArray
-- array ((0,0),(9,9)) [((0,0),'.'),((0,1),'.'),((0,2),'@'),((0,3),'@'),((0,4),'.'),((0,5),'@'),((0,6),'@'),((0,7),'@'),((0,8),'@'),((0,9),'.'),((1,0),'@'),((1,1),'@'),((1,2),'@'),((1,3),'.'),((1,4),'@'),((1,5),'.'),((1,6),'@'),((1,7),'.'),((1,8),'@'),((1,9),'@'),((2,0),'@'),((2,1),'@'),((2,2),'@'),((2,3),'@'),((2,4),'@'),((2,5),'.'),((2,6),'@'),((2,7),'.'),((2,8),'@'),((2,9),'@'),((3,0),'@'),((3,1),'.'),((3,2),'@'),((3,3),'@'),((3,4),'@'),((3,5),'@'),((3,6),'.'),((3,7),'.'),((3,8),'@'),((3,9),'.'),((4,0),'@'),((4,1),'@'),((4,2),'.'),((4,3),'@'),((4,4),'@'),((4,5),'@'),((4,6),'@'),((4,7),'.'),((4,8),'@'),((4,9),'@'),((5,0),'.'),((5,1),'@'),((5,2),'@'),((5,3),'@'),((5,4),'@'),((5,5),'@'),((5,6),'@'),((5,7),'@'),((5,8),'.'),((5,9),'@'),((6,0),'.'),((6,1),'@'),((6,2),'.'),((6,3),'@'),((6,4),'.'),((6,5),'@'),((6,6),'.'),((6,7),'@'),((6,8),'@'),((6,9),'@'),((7,0),'@'),((7,1),'.'),((7,2),'@'),((7,3),'@'),((7,4),'@'),((7,5),'.'),((7,6),'@'),((7,7),'@'),((7,8),'@'),((7,9),'@'),((8,0),'.'),((8,1),'@'),((8,2),'@'),((8,3),'@'),((8,4),'@'),((8,5),'@'),((8,6),'@'),((8,7),'@'),((8,8),'@'),((8,9),'.'),((9,0),'@'),((9,1),'.'),((9,2),'@'),((9,3),'.'),((9,4),'@'),((9,5),'@'),((9,6),'@'),((9,7),'.'),((9,8),'@'),((9,9),'.')]
testArray = buildArray testCase

(!?) :: Array (Int,Int) e -> (Int,Int) -> Maybe e
(!?) arr idx = if inBounds arr idx then Just $ arr ! idx else Nothing
  where inBounds a (y,x) =
          let ((miny, minx),(mxy, mxx)) = bounds a
           in (y >= miny) && (x >= minx) && (y <= mxy) && (x <= mxx)
