{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text hiding (length, map)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (transpose)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    file <- pack <$> readFile "day6/input/day_6.txt"
    putStrLn "Part 1: "
    print $ part1 file
    putStrLn "Part 2: "
    print $ part2 file

part1 :: Text -> Int
part1 input =
  case parse parseInput "" input of
    (Left e) -> error $ show e
    (Right (vals,ops)) -> sum $ Prelude.zipWith solve (Data.List.transpose vals) ops
  where solve vals Times = product vals
        solve vals Add = sum vals

type Parser a = Parsec Text Text a

data Op = Times | Add
  deriving (Show, Eq)

parseOp :: Parser Op
parseOp = Times <$ char '*' <|> Add <$ char '+'

lexeme = L.lexeme spaceConsumer

int :: Parser Int
int = lexeme L.decimal


op :: Parser Op
op = lexeme  parseOp

spaceConsumer :: Parser ()
spaceConsumer = L.space hspace1 (L.skipLineComment "--") Text.Megaparsec.empty

parseInput :: Parser ([[Int]], [Op])
parseInput = do
  valueRows <- many (sepBy1 int spaceConsumer <* eol)
  finalRow <- sepBy1 op spaceConsumer <* eof
  return (valueRows, finalRow)

testCase1 =
  "123 328  51 64\n\
  \45 64  387 23\n\
  \6 98  215 314\n\
  \*   +   *   +"


testCase =
  "123 328  51 64\n\
  \45 64  387 23\n\
  \6 98  215 314\n\
  \*   +   *   +"

-- >>> testPart1
-- 4277556
testPart1 = part1 testCase

part2 :: Text -> Int
part2 input =
  case parse parseInput "" input of
    (Left e) -> error $ show e
    (Right (vals,ops)) -> sum $ Prelude.zipWith solve' (Data.List.transpose vals) ops

solve' :: [Int] -> Op -> Int
solve' vals op = go op $ cols $ pad vals
  where pad :: [Int] -> [String]
        pad xs =
          let longest = Prelude.maximum $ map length strings
              strings = map show xs
           in map (\a -> Prelude.replicate (longest - (length a)) ' ' <> a) strings

        cols :: [String] -> [Int]
        cols = map read . Data.List.transpose

        go Times vals = product vals
        go Add vals = sum vals


-- >>> testPart2
-- 3264322
testPart2 = part2 testCase
