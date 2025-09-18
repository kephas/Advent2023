{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Day05 where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Paths_aoc2023 (getDataFileName)
import Text.Megaparsec
import Text.Megaparsec.Char (lowerChar, space)
import Text.Megaparsec.Char.Lexer qualified as L

day05 :: IO ()
day05 = day05_1 "test"

day05_1 :: String -> IO ()
day05_1 kind = do
    file <- dataPath kind
    input <- readFile file
    let result = parse almanacP file input
    case result of
        Left bundle ->
            putStrLn $ errorBundlePretty bundle
        Right almanac ->
            print $ minimum $ map (lookupAlmanac almanac) almanac.seeds

transitions :: [(String, String)]
transitions = pairs $ words "seed soil fertilizer water light temperature humidity location"

lookupAlmanac :: Almanac -> Int -> Int
lookupAlmanac almanac input =
    foldl (lookupAlmanacMap almanac) input transitions

lookupAlmanacMap :: Almanac -> Int -> (String, String) -> Int
lookupAlmanacMap almanac input (source, dest) =
    --              : Maybe (Int -> Int)                     <*> Maybe Int  --> Maybe Int
    fromMaybe input $ Map.lookup (source, dest) almanac.maps <*> Just input

lookupAlmanacMap' :: Almanac -> Int -> (String, String) -> Int
lookupAlmanacMap' almanac input (source, dest) =
    fromMaybe input $ do
        mapper <- Map.lookup (source, dest) almanac.maps
        pure $ mapper input

data Almanac
    = Almanac
    { seeds :: [Int]
    , maps :: Map (String, String) (Int -> Int)
    }

type Parser = Parsec Void String

almanacP :: Parser Almanac
almanacP = do
    seeds <- symbol "seeds:" *> some int
    Almanac seeds . Map.fromList <$> some mapP

mapP :: Parser ((String, String), Int -> Int)
mapP = do
    source <- try wordP
    void $ chunk "-to-"
    dest <- wordP
    space
    void $ symbol "map:"
    chainLookup <- foldl (>=>) pure <$> some rangeP
    pure ((source, dest), either id id . chainLookup)
  where
    wordP :: Parser String
    wordP = some lowerChar
    rangeP :: Parser (Int -> Either Int Int)
    rangeP = do
        destStart <- int
        sourceStart <- int
        len <- int
        let sourceEnd = sourceStart + len - 1
            shift = destStart - sourceStart
            lookupRange num =
                if num >= sourceStart && num <= sourceEnd
                    then Left (num + shift) -- no further lookup
                    else Right num
        pure lookupRange

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

int :: Parser Int
int = lexeme L.decimal

dataPath :: String -> IO FilePath
dataPath kind = getDataFileName $ "day05-" <> kind <> ".txt"

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs [x1, x2] = [(x1, x2)]
pairs (x1 : next@(x2 : _)) = (x1, x2) : pairs next
