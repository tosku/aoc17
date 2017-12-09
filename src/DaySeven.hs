{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module DaySeven
    ( solution
    ) where

import System.Environment
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Csv
import Data.List.Split

type Disk = String
type DiskTree = M.Map Disk (Int, [Disk])

readInput :: IO DiskTree
readInput = do
  lns <- fmap lines (readFile "./inputs/d7")
  let dch = map (splitOn " -> ") lns
  dtr <- mapM (\ln -> do
    let ds = (splitOn " (" (head ln))
    let nn = head ds
    let we =  read $ filter (/=')') (last ds)
    let ch = if length ln == 1
        then []
        else last ln
    let out = (nn, (we, splitOn ", " ch))
    return out) dch
  return $ M.fromList dtr

type Parents = M.Map String String
makeParents :: DiskTree -> Parents
makeParents dt = 
  M.foldlWithKey (\ac n (w,ns) -> 
    foldl' (\ac' c -> M.insert c n ac') ac ns) M.empty dt

getBase :: Parents -> Disk
getBase prs = 
  let workit n =
        case M.lookup n prs of
          Nothing -> n
          Just c -> workit c
   in workit $ snd (M.findMin prs)

solvePartI :: IO ()
solvePartI = do
  ins <- readInput
  putStrLn "Part I"
  let prnts = makeParents ins
  let base = getBase $ makeParents ins
  putStrLn $ show base

type Levels = IM.IntMap (S.Set Disk)
makeLevels :: DiskTree -> Levels
makeLevels dt = 
  let base = getBase $ makeParents dt
      workit :: Disk -> Int -> Levels -> Levels
      workit n l lvs =
        let l' = l + 1
            (w,cs) = fromJust $ M.lookup n dt
            lvs' = (IM.insert l' (S.singleton n) lvs)
         in if cs == [""]
               then lvs
               else IM.unionsWith S.union $ map (\c -> workit c l' lvs') cs
   in workit base 0 IM.empty

getWeight :: DiskTree -> Disk -> Int
getWeight dt d =
  let workit "" = 0
      workit n =
        let (w,cs) = fromJust $ M.lookup n dt
         in w + sum (map workit cs)
   in workit d

weightChildren :: DiskTree -> M.Map Disk Int -> Disk -> (Disk,Int)
weightChildren dt weights n =
  let (w,cs) = fromJust $ M.lookup n dt
      gws = map (\s -> (s,getWeight dt s)) cs
      grws = groupBy (\(s1,w1) (s2,w2) -> w1 == w2) gws
      diff = filter (\l -> length l == 1) grws
      same = head $ head $ filter (\l -> length l /= 1) grws
   in if null diff
         then ("",0)
         else 
           let df = head $ head diff
               (ow,cs) = fromJust $ M.lookup (fst df) dt
            in (fst df, snd same - snd df + ow)

solvePartII :: IO ()
solvePartII = do
  putStrLn "Part II"
  ins <- readInput
  let weights = M.mapWithKey (\k a -> getWeight ins k) ins
  let levels = makeLevels ins
  let base = getBase $ makeParents ins
  let workit :: Int -> (Disk, Int)
      workit l =
        let level = fromJust $ IM.lookup l levels
            diffs = map (\n -> weightChildren ins weights n) $ S.toList level
            df = filter (\(d,w) -> w /=0) diffs
         in if null df 
               then workit (l-1)
               else head df
  putStrLn $ show $ workit $ fst $ IM.findMax levels

solution :: IO ()
solution = do
  solvePartI
  solvePartII

