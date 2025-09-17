{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}

module Day03 where

import Control.Monad (guard)
import Data.Char qualified as C
import Data.Foldable (toList)
import Data.List (nub)
import Data.Matrix qualified as M
import Data.Maybe (mapMaybe)

-- import Data.Set qualified as S
-- import Debug.Trace (traceShowM)
import Paths_aoc2023 (getDataFileName)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Core.Runner (defaultConfig, evaluateSummary, runSpec)

day03 :: IO ()
day03 = day03_1 >> day03_2

day03_1 :: IO ()
day03_1 = do
  total <- sum . walkEngine . mkEngine <$> (getDataFileName "day03-input.txt" >>= readFile)
  putStr "day 03/1: "
  print total

day03_2 :: IO ()
day03_2 = do
  _total <- sum . walkGears . mkEngine <$> (getDataFileName "day03-input.txt" >>= readFile)
  putStr "day 03/2: "
  print $ sum $ walkGears $ mkEngine toy
  gearsTests

newtype Engine = Engine (M.Matrix Char)

type Coord = (Int, Int)

data SchematicPart
  = Empty Coord
  | Symbol Coord Char
  | Number [Coord] Int
  deriving (Eq, Show)

isNumber :: SchematicPart -> Bool
isNumber (Number _ _) = True
isNumber _ = False

toInt :: SchematicPart -> Maybe Int
toInt (Number _ num) = Just num
toInt _ = Nothing

walkEngine :: Engine -> [Int]
walkEngine engine =
  go (1, 1) []
 where
  go :: Coord -> [Int] -> [Int]
  go coord acc =
    case getCoord engine coord of
      Nothing -> if beginningOfLine coord then acc else go (nextRow coord) acc
      (justDigit -> False) -> go (nextCol coord) acc
      (justDigit -> True) ->
        let (next, mNum) = readNumber engine coord
         in go next (toList mNum ++ acc)

walkGears :: Engine -> [Int]
walkGears engine =
  go (1, 1) $ map0 engine
 where
  map0 (Engine matrix) = M.matrix (M.nrows matrix) (M.ncols matrix) Empty
  go :: Coord -> M.Matrix SchematicPart -> [Int]
  go coord gearMap =
    case getCoord engine coord of
      Nothing -> if beginningOfLine coord then extractGears gearMap else go (nextRow coord) gearMap
      Just _ ->
        case readNumber' engine coord of
          (next, Nothing) -> go (nextCol next) gearMap
          (next, Just part) -> go next $ storePart gearMap part

extractGears :: M.Matrix SchematicPart -> [Int]
extractGears gearMap =
  mapMaybe (partToScore gearMap) $ M.toList gearMap

partToScore :: M.Matrix SchematicPart -> SchematicPart -> Maybe Int
partToScore gearMap (Symbol coord char)
  | isSymbol char = do
      let numbers = nub $ filter isNumber $ getNeighbours gearMap coord
      guard $ length numbers == 2
      Just $ product $ mapMaybe toInt numbers
partToScore _ _ = Nothing

storePart :: M.Matrix SchematicPart -> SchematicPart -> M.Matrix SchematicPart
storePart matrix empty@(Empty coord) = M.setElem empty coord matrix
storePart matrix sym@(Symbol coord _char) = M.setElem sym coord matrix
storePart matrix num@(Number coords _number) =
  foldr (M.setElem num) matrix coords

readNumber :: Engine -> Coord -> (Coord, Maybe Int)
readNumber engine (row, col0) =
  go "" col0 ""
 where
  go :: String -> Int -> String -> (Coord, Maybe Int)
  go num col symbols =
    case getCoord engine (row, col) of
      Nothing ->
        ((row + 1, 1), result num symbols)
      Just char
        | C.isDigit char ->
            go (num ++ [char]) (col + 1) $ symbols ++ getNeighbours engine (row, col)
      Just _ -> ((row, col), result num symbols)
  result num symbols =
    if keepSymbols symbols == ""
      then Nothing
      else Just $ read num

readNumber' :: Engine -> Coord -> (Coord, Maybe SchematicPart)
readNumber' engine (row, start) =
  case readNumber engine (row, start) of
    ((_, end), Just num) -> ((row, end), Just $ Number coords num)
     where
      coords = [(row, c) | c <- [start .. end - 1]]
    (end, Nothing) -> (end, Nothing)

getNeighbours :: (Matrixable m a) => m -> (Int, Int) -> [a]
getNeighbours matrix (row, col) =
  mapMaybe (getCoord matrix . addXY (row, col)) neighbourCoords
 where
  addXY (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

neighbourCoords :: [(Int, Int)]
neighbourCoords = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

keepSymbols :: String -> String
keepSymbols = filter isSymbol

isSymbol :: Char -> Bool
isSymbol = (`notElem` "0123456789.")

toy :: String
toy =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."

toySum :: Int
toySum = 4361

-- Utils

class Matrixable m a | m -> a where
  getCoord :: m -> Coord -> Maybe a

instance Matrixable (M.Matrix a) a where
  getCoord matrix (row, col) =
    M.safeGet row col matrix

instance Matrixable Engine Char where
  getCoord (Engine matrix) =
    getCoord matrix

beginningOfLine :: Coord -> Bool
beginningOfLine (_, 1) = True
beginningOfLine _ = False

nextRow, nextCol :: Coord -> Coord
nextRow (row, _) = (row + 1, 1)
nextCol (row, col) = (row, col + 1)

mkEngine :: String -> Engine
mkEngine = Engine . M.fromLists . lines

justDigit :: Maybe Char -> Bool
justDigit Nothing = False
justDigit (Just c) = C.isDigit c

-- | Tests
gearMap1 :: M.Matrix SchematicPart
gearMap1 =
  M.fromLists
    [ [Symbol (1, 1) '.', gear11, Number [(1, 3), (1, 4)] 14, Number [(1, 3), (1, 4)] 14, gear12]
    , [Number [(2, 1), (2, 2)] 23, Number [(2, 1), (2, 2)] 23, Symbol (2, 3) '.', Number [(2, 4), (2, 5)] 32]
    , [Symbol (3, 1) '#', Symbol (3, 2) '.', Symbol (3, 3) '.', Symbol (3, 4) '.', Symbol (3, 5) '+']
    ]

gear11, gear12 :: SchematicPart
gear11 = Symbol (1, 2) '*'
gear12 = Symbol (1, 5) '^'

gearsTests :: IO ()
gearsTests =
  runTests $
    describe "gears" $ do
      it "reads numbers" $ do
        readNumber (mkEngine toy) (1, 1) `shouldBe` ((1, 4), Just 467)
        readNumber (mkEngine toy) (1, 4) `shouldBe` ((1, 4), Nothing)
      it "gets gears" $ do
        partToScore gearMap1 gear11 `shouldBe` Just (14 * 23)
        partToScore gearMap1 gear12 `shouldBe` Just (14 * 32)
 where
  runTests tests = runSpec tests defaultConfig >>= evaluateSummary
