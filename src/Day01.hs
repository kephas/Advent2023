{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use last" #-}
module Day01 where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, stripPrefix, uncons, unfoldr)
import Data.Maybe (fromJust)
import Paths_aoc2023 (getDataFileName)

day01 :: IO ()
day01 = day01_1 >> day01_2

day01_1 :: IO ()
day01_1 = do
  values <- map (readNumber . getDigits) . lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStr "day 01/1: "
  print $ sum values
 where
  readNumber str = read [head str, last str] :: Int
  getDigits = filter isDigit

day01_2 :: IO ()
day01_2 = do
  values <- map readNumber . lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStr "day 01/2: "
  print $ sum values
 where
  readNumber str = head (getDigits str) * 10 + last (getDigits str)
  getDigits = unfoldr readOneDigit
  readOneDigit :: String -> Maybe (Int, String)
  readOneDigit str =
    case hasPrefixFrom digitWords str of
      Just (digit@(_ : rest1), rest2) -> Just (toInt digit, rest1 ++ rest2)
      Just ("", rest) -> readOneDigit rest
      Nothing -> case uncons str of
        Just (c, rest) | isDigit c -> Just (digitToInt c, rest)
        Just (_, rest) -> readOneDigit rest
        Nothing -> Nothing
  hasPrefixFrom :: [String] -> String -> Maybe (String, String)
  hasPrefixFrom digits str = foldr (\digit result -> result <|> ((digit,) <$> stripPrefix digit str)) Nothing digits
  digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  toInt digit = 1 + fromJust (elemIndex digit digitWords)
