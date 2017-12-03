module Main where

import System.Environment
import DayOne
import DayTwo
import DayThree

main :: IO ()
main = do
  args <- getArgs
  let day = if length args == 1
      then args !! 0
      else "error"
  putStrLn "solving day: "
  putStrLn day
  case day of 
    "1" -> solveDayOne
    "2" -> solveDayTwo
    "3" -> solveDayThree
    _   -> putStrLn "you must pick a day to solve ex `aoc17-exe 3`"
  return ()
