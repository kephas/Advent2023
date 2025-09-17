{-# LANGUAGE ImportQualifiedPost #-}

module Day02 where

import Data.Functor (void)
import Data.Maybe (mapMaybe)
import Paths_aoc2023 (getDataFileName)
import Text.Megaparsec (Parsec, parseMaybe, try, (<|>))
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Parser.Combinators (many, sepBy)

day02 :: IO ()
day02 = do
  day02_1
  day02_2

day02_1 :: IO ()
day02_1 = do
  games <- mapMaybe (checkGame reference) <$> readGames
  putStr "day 02/1: "
  print $ sum games

reference :: Handful
reference = Handful 12 13 14

day02_2 :: IO ()
day02_2 = do
  games <- readGames
  putStr "day 02/2: "
  print $ sum $ map minPower games

data Game = Game Int [Handful] deriving (Show)

checkGame :: Handful -> Game -> Maybe Int
checkGame hand (Game num hands) =
  if all (`isPossibleWith` hand) hands
    then Just num
    else Nothing

minPower :: Game -> Int
minPower (Game _ hands) =
  power $ foldr needed empty hands

readGames :: IO [Game]
readGames = mapMaybe (parseMaybe game) . lines <$> (getDataFileName "day02-input.txt" >>= readFile)

data Handful = Handful {red, green, blue :: Int} deriving (Show)

empty :: Handful
empty = Handful 0 0 0

isPossibleWith :: Handful -> Handful -> Bool
(Handful r1 g1 b1) `isPossibleWith` (Handful r2 g2 b2) =
  r1 <= r2 && g1 <= g2 && b1 <= b2

needed :: Handful -> Handful -> Handful
needed (Handful r1 g1 b1) (Handful r2 g2 b2) =
  Handful (max r1 r2) (max g1 g2) (max b1 b2)

power :: Handful -> Int
power (Handful r g b) = r * g * b

game :: Parser Game
game = do
  void $ symbol "Game"
  num <- integer
  void $ symbol ":"
  Game num <$> handful `sepBy` symbol ";"

handful :: Parser Handful
handful =
  foldr applyColor empty <$> color `sepBy` symbol ","

data Color = Color Int String deriving (Show)

color :: Parser Color
color = Color <$> integer <*> (try (symbol "red") <|> try (symbol "green") <|> symbol "blue")

applyColor :: Color -> Handful -> Handful
applyColor (Color count "red") hand = hand{red = count}
applyColor (Color count "green") hand = hand{green = count}
applyColor (Color count "blue") hand = hand{blue = count}
applyColor _ hand = hand

symbol :: String -> Parser String
symbol = L.symbol space

integer :: Parser Int
integer = lexeme L.decimal

space :: Parser ()
space = void $ many C.space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

type Parser a = Parsec () String a
