{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import qualified Data.IntSet as S
import Data.Foldable (foldl')
import Data.List (sort)

main :: IO ()
main = do
    file <- pack <$> readFile "day5/input/day_5.txt"
    putStrLn "Part 1: "
    print $ part1 file
    putStrLn "Part 2: "
    print $ part2 file


type Range = (Int, Int)
type Ingredient = Int

isInRange :: Range -> Ingredient -> Bool
isInRange (s, e) i = i >= s && i <= e

isFresh :: [Range] -> Ingredient -> Bool
isFresh rs i = any (`isInRange` i) rs

type Parser a = Parsec Void Text a

part1 :: Text -> Int
part1 xs = case parse parseInput "" xs of
  Left _ -> error "no parse"
  Right (rs, is) -> length $ filter (isFresh rs) is

parseRange :: Parser Range
parseRange = do
  start <- decimal
  _ <- char '-'
  end <-  decimal
  _ <- eol
  return $  (start, end)

parseIngredient :: Parser Ingredient
parseIngredient =
  decimal <* eol

parseInput :: Parser ([Range], [Ingredient])
parseInput = do
  ranges <- many parseRange
  _ <- char '\n'
  ingredients <- manyTill parseIngredient eof
  return (ranges,ingredients)


part2 :: Text -> Int
part2 xs = case parse parseInput "" xs of
  Left _ -> error "no parse"
  Right (rs, _) -> foldr (\(s,e) acc -> acc + (e - s + 1)) 0 (merge $ sort rs)

merge :: Ord a => [(a, a)] -> [(a, a)] -- merge ranges together (list must be sorted)
merge [] = []
merge [x] = [x]                        -- vvv if range start overlaps, merge it
merge (r1@(s1,e1):r2@(s2,e2):rs) | e1 >= s2  = merge $ (s1, max e1 e2) : rs
                                 | otherwise = r1 : merge (r2 : rs)


rangeToInts :: Range -> S.IntSet
rangeToInts (s, e) = S.fromRange (s, e)

testCase :: Text
testCase =
  "3-5\n\
\10-14\n\
\16-20\n\
\12-18\n\
\\n\
\1\n\
\5\n\
\8\n\
\11\n\
\17\n\
\32\n"


-- >>> testPart1
-- 3
testPart1 = part1 testCase

-- >>> testPart2
-- 14
testPart2 = part2 testCase
