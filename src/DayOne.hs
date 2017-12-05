{-# LANGUAGE ScopedTypeVariables #-}
module DayOne
    ( solution
    ) where

import System.Environment
import Data.Char
import qualified Data.IntMap as IM
import Data.Maybe

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

solution :: IO ()
solution = do
  ins <- readInput
  solvePartI ins
  solvePartII ins

solvePartI :: [Int] -> IO ()
solvePartI ins = do
  let offset = if head ins == last ins 
      then head ins
      else 0
  let respartI = sumClone (head ins) (tail ins) offset
  putStrLn "part I:"
  putStrLn $ show respartI
    where 
      sumClone x [] out = out
      sumClone x (n:xs) out = 
        if x == n 
           then sumClone n xs (out+x)
           else sumClone n xs out

solvePartII :: [Int] -> IO ()
solvePartII ins' = do
  putStrLn "part II:"
  putStrLn $ show $ sumAntipodes n 0
    where 
      ins = IM.fromList $ zip [1..] ins'
      n = IM.size ins
      next i = let ofs = mod (i + (quot n 2)) n
                in if ofs == 0
                      then n
                      else ofs
      sumAntipodes 0 out = out
      sumAntipodes i out = 
        let vi = fromJust $ IM.lookup i ins
            antii = fromJust $ IM.lookup (next i) ins
         in if vi == antii  
                 then sumAntipodes (i-1) (out+vi)
                 else sumAntipodes (i-1) out
