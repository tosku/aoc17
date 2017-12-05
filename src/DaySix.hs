{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module DaySix
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
  ins <- readLines "./inputs/d6"
  putStrLn $ show ins
  putStrLn "Part I"

solvePartII :: IO ()
solvePartII = do
  putStrLn "Part II"

solution :: IO ()
solution = do
  solvePartI
  solvePartII

