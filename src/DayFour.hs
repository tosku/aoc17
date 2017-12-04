{-# LANGUAGE ScopedTypeVariables #-}
module DayFour
    ( solveDayFour
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

myOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord ' ')
    }

readInput :: IO [[String]]
readInput = do
  csvData <- BL.readFile "./inputs/d4"
  out <- case decodeWith myOptions NoHeader csvData of
              Left err -> do
                putStrLn err
                return $ V.singleton V.empty
              Right rows -> 
                return rows
  return $ V.toList $ V.map V.toList out

solvePartI :: IO ()
solvePartI = do
  ins <- readInput
  putStrLn $ show ins
  let result = length 
                $ filter (\pass -> length pass == S.size (S.fromList pass)) ins
  putStrLn "Part I"
  putStrLn $ show result

solvePartII :: IO ()
solvePartII = do
  ins' <- readInput
  let ins = map ( map $ sort) ins'
  let result = length 
                $ filter (\pass -> length pass == S.size (S.fromList pass)) ins
  putStrLn "Part II"
  putStrLn $ show result

solveDayFour :: IO ()
solveDayFour = do
  solvePartI
  solvePartII

