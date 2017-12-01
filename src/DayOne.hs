{-# LANGUAGE ScopedTypeVariables #-}
module DayOne
    ( solveDayOne
    ) where

import System.Environment
import Data.Char

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [Int]
makeInteger = map read


readInput :: IO [Int]
readInput = do
  content <- readFile "./inputs/d1"
  let rstrip = reverse . dropWhile isSpace . reverse
  let intlist = map digitToInt (rstrip content)
  putStrLn content
  return intlist

solveDayOne :: IO Int
solveDayOne = do
  ins <- readInput
  let offset = if head ins == last ins 
      then head ins
      else 0
  let res = sumClone (head ins) (tail ins) offset
  putStrLn $ show res
  return res
    where 
      sumClone x [] out = out
      sumClone x (n:xs) out = 
        if x == n 
           then sumClone n xs (out+x)
           else sumClone n xs out

