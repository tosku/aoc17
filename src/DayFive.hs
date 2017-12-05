{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module DayFive
    ( solution
    ) where

import System.Environment
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

readLines :: FilePath -> IO [Int]
readLines fp = do
  lns <- fmap lines (readFile fp)
  return $ map read lns

solvePartI :: IO ()
solvePartI = do
  ins <- readLines "./inputs/d5"
  let instructions = IM.fromList $ zip [1..] ins
  putStrLn $ show instructions
  let followInstruction :: Int -> Int -> IM.IntMap Int -> Int
      followInstruction i steps instrs =
        case IM.lookup i instrs of
          Nothing -> steps
          Just j -> 
            let instrs' = IM.adjust ((+) 1) i instrs
             in followInstruction (i+j) (steps+1) instrs'
  putStrLn "Part I"
  putStrLn $ show $ followInstruction 1 0 instructions

solvePartII :: IO ()
solvePartII = do
  ins <- readLines "./inputs/d5"
  let instructions = IM.fromList $ zip [1..] ins
  let followInstruction :: Int -> Int -> IM.IntMap Int -> Int
      followInstruction !i !steps !instrs =
        case IM.lookup i instrs of
          Nothing -> steps
          Just j -> 
            let instrs' = IM.adjust (\j -> if j >= 3 then j - 1 else j + 1) i instrs
             in followInstruction (i+j) (steps+1) instrs'
  putStrLn "Part II"
  putStrLn $ show $ followInstruction 1 0 instructions

solution :: IO ()
solution = do
  solvePartI
  solvePartII

