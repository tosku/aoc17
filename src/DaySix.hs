{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module DaySix
    ( solution
    ) where

import System.Environment
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.List.Split

readLines :: FilePath -> IO [Int]
readLines fp = do
  lns <- fmap (head . lines) (readFile fp)
  return $ map read $ splitOn "\t" lns

next :: Int -> Int
next 16 = 1
next i = i+1

redistribute :: IM.IntMap Int -> IM.IntMap Int
redistribute banks =
  let (rest,maxIndx) = IM.foldlWithKey' (\(rst,idx) k v -> 
                          if v > rst 
                             then (v,k)
                             else (rst,idx)
                         ) (0,0) banks
      banks' = IM.adjust (const 0) maxIndx banks 
      redis b 0 _ = b
      redis b r i = 
        let i' = next i
            b' = IM.adjust ((+) 1) i' b
            r' = r - 1
         in redis b' r' i'
   in redis banks' rest maxIndx

workit :: IM.IntMap Int -> M.Map (IM.IntMap Int) Int -> IO (Int,Int)
workit banks hist = do
  let size = M.size hist
  case M.lookup banks hist of
    Just t -> return $ (size,size - t)
    Nothing -> do
         let banks' = redistribute banks
         let hist' = M.insert banks size hist
         workit banks' hist'

solution :: IO ()
solution = do
  ins <- readLines "./inputs/d6"
  let banks = IM.fromList $ zip [1..] ins
  let history = M.empty
  putStrLn $ show ins
  putStrLn "Part I"
  out <- workit banks history
  putStrLn $ show $ fst out
  putStrLn "Part II"
  putStrLn $ show $ snd out

