module Main where

import Day01
import Day02
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
    [] -> days
    _ -> error "None or invalid day number provided."

days :: IO ()
days = day01 >> day02
