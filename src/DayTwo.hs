{-# LANGUAGE ScopedTypeVariables #-}
module DayTwo
    ( solution
    ) where

import System.Environment
import Data.Char
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

myOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord '\t')
    }

readInput :: IO (V.Vector (V.Vector Int))
readInput = do
  csvData <- BL.readFile "./inputs/d2"
  out <- case decodeWith myOptions NoHeader csvData of
              Left err -> do
                putStrLn err
                return $ V.singleton V.empty
              Right rows -> 
                return rows
  return out

solvePartI :: IO ()
solvePartI = do
  ins <- readInput
  let result = V.sum $ V.map (\l -> 
                  let mn = V.minimum l
                      mx = V.maximum l
                   in mx - mn) ins
  putStrLn $ show ins
  putStrLn "Part I"
  putStrLn $ show result

solvePartII :: IO ()
solvePartII = do
  ins' <- readInput
  let ins = map V.toList  (V.toList ins') :: [[Int]]
  let makePairs l = (,) <$> l <*> l
  let pairres p = 
        let x = fst p
            y = snd p
         in if mod x y == 0 && x /= y
               then quot x y
               else 0
  let result = sum $ map (\l -> 
                 sum $ map pairres $ makePairs l
                         ) ins
  putStrLn "Part II"
  putStrLn $ show result

solution :: IO ()
solution = do
  solvePartI
  solvePartII

