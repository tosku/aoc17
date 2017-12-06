module Main where

import System.Environment
import qualified DayOne as One
import qualified DayTwo as Two
import qualified DayThree as Three
import qualified DayFour as Four
import qualified DayFive as Five
import qualified DaySix as Six
import qualified DaySeven as Seven
import qualified DayEight as Eight
import qualified DayNine as Nine

main :: IO ()
main = do
  args <- getArgs
  let day = if length args == 1
      then args !! 0
      else "error"
  putStrLn "solving day: "
  putStrLn day
  case day of 
    "1" -> One.solution
    "2" -> Two.solution
    "3" -> Three.solution
    "4" -> Four.solution
    "5" -> Five.solution
    "6" -> Six.solution
    "7" -> Seven.solution
    "8" -> Eight.solution
    "9" -> Nine.solution
    _   -> putStrLn "you must pick a day to solve ex `aoc17-exe 3`"
  return ()
