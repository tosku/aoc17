{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module DayEight
    ( solution
    ) where

import System.Environment
import Data.Char
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.List.Split

type Register = String

type Memory = M.Map Register Int

data Pred = Pred Register (Int -> Bool)

runPred :: Memory -> Pred -> Bool
runPred m (Pred reg condition) =
  let rv = M.lookup reg m
   in case rv of
        Nothing -> condition 0
        Just v -> condition v

data Command = Command Register (Int -> Int)

getReg :: Command -> Register
getReg (Command r _) = r

runCommand :: Memory -> Command -> Memory
runCommand m (Command reg cmd) =
  let mv = M.lookup reg m
   in case mv of
        Just v -> M.adjust cmd reg m
        Nothing -> M.insert reg (cmd 0) m

data Row = Row { condition :: Pred
               , command :: Command
               }
               deriving (Show)

instance Show Command where
  show (Command reg cmd) = reg

instance Show Pred where
  show (Pred reg condition) = reg

readRow :: String -> Row
readRow line =
  let l = splitOn " if " line
      comstr = head l
      prestr = last l
      readCommand cs = 
        let css = splitOn " " cs
            rg = head css
            am = read $ last css :: Int
            cm = if css !! 1 == "inc"
                    then ((+) am)
                    else ((+) (-am))
         in Command rg cm
      readPred pstr = 
        let ps = splitOn " " pstr
            rg = head ps
            am = read $ last ps :: Int
            cm = case ps !! 1 of
                   "==" -> (\v -> v == am)
                   "!=" -> (\v -> v /= am)
                   ">" -> (\v -> v > am)
                   ">=" -> (\v -> v >= am)
                   "<" -> (\v -> v < am)
                   "<=" -> (\v -> v <= am)
         in Pred rg cm
   in Row { condition = readPred prestr
          , command = readCommand comstr
          }


readLines :: FilePath -> IO [Row]
readLines fp = do
  lns <- fmap lines (readFile fp)
  return $ map readRow lns

solvePartI :: IO ()
solvePartI = do
  ins <- readLines "./inputs/d8"
  putStrLn "Part I"
  {-putStrLn $ show ins-}
  let mem = foldl (\ac row -> 
            if runPred ac (condition row)
               then runCommand ac (command row)
               else ac) M.empty ins
  let max = maximum $ map snd $ M.toList mem
  putStrLn $ show mem
  putStrLn $ show max

solvePartII :: IO ()
solvePartII = do
  putStrLn "Part II"
  ins <- readLines "./inputs/d8"
  let mem = foldl (\(mx,ac) row -> 
            if runPred ac (condition row)
               then 
                 let !nac = runCommand ac (command row)
                     nv = fromJust $ M.lookup (getReg (command row)) nac
                  in (max mx nv, nac)
               else (mx,ac)
                 ) (0, M.empty) ins
  putStrLn $ show mem

solution :: IO ()
solution = do
  solvePartI
  solvePartII

