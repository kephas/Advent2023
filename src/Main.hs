module Main where

import Day01
import Day02
import Day03
import Day05
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        "1" : "1" : _ -> day01_1
        "1" : "2" : _ -> day01_2
        "1" : _ -> day01
        "2" : "1" : _ -> day02_1
        "2" : "2" : _ -> day02_2
        "2" : _ -> day02
        "3" : "1" : _ -> day03_1
        "3" : "2" : _ -> day03_2
        "3" : _ -> day03
        "51/" : _ -> day05_1 "test"
        "51" : _ -> day05_1 "input"
        [] -> days
        _ -> error $ "None or invalid day number provided: " <> show args

days :: IO ()
days = day01 >> day02 >> day03
