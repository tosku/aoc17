module Main where

import System.Environment
import DayOne

main :: IO ()
main = do
  args <- getArgs
  let day = if length args == 1
      then args !! 0
      else "error"
  putStrLn "solving day: "
  putStrLn day
  case day of 
    "1" -> do
      solveDayOne
      return ()
    _ -> putStrLn "you must pick a day to solve ex `aoc17-exe 3`"
  return ()
