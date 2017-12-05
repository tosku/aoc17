{-# LANGUAGE ScopedTypeVariables #-}
module DayThree
    ( solution
    ) where

import System.Environment
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.List

input = 325489 :: Int
t1 = 23 :: Int
t2 = 4 :: Int
t5 = 18 :: Int

data Direction = D|DL|L|LU|U|UR|R|RD
  deriving (Show,Eq,Enum)

prevCorner :: Direction -> Direction
prevCorner DL = RD
prevCorner LU = DL
prevCorner UR = LU
prevCorner RD = UR

nextDirection :: Direction -> Direction
nextDirection D = L
nextDirection L = U
nextDirection U = R
nextDirection R = D

rdCornerSquareroot i =
  let findSquare i r
        | r * r >= i = r
        | otherwise = 
            let r' = r+2
             in findSquare i r'
  in findSquare i 1

corner :: Int -> Direction -> Int
corner i RD = (rdCornerSquareroot i)^2 
corner i d  = corner i (prevCorner d) - side i

radius i = quot (rdCornerSquareroot i - 1) 2

perimeter i = 
  let rdcorn = rdCornerSquareroot i
   in rdcorn^2 - (rdcorn-2)^2

side i = quot (perimeter i) 4

getNeighbor :: Int -> Direction -> Int
getNeighbor 1 d  
  | d == D = 8
  | d == L = 6
  | d == U = 4
  | d == R = 2
getNeighbor i d 
  | d == D = case icorn of
               (RD, r) -> nextRD - r - 1
               (DL, 0) -> nextRD - (side nextRD) + 1
               (DL, r) -> i + 1
               (LU, 0) -> i + 1
               (LU, r) -> corner prevRD LU - r + 1 
               (UR, r) -> if i == firstinsq 
                             then corner i RD
                             else i - 1
  | d == L = case icorn of
               (RD, r) -> i - 1
               (DL, r) -> corner nextRD DL - r - 1
               (LU, 0) -> corner nextRD DL - (side nextRD) + 1
               (LU, r) -> i + 1 
               (UR, 0) -> i + 1
               (UR, r) -> if i == firstinsq 
                             then prevRD
                             else corner prevRD UR - r + 1
  | d == U = case icorn of
               (RD, r) -> if i == corner i RD 
                             then prevRD + 1
                             else prevRD - r + 1
               (DL, r) -> i - 1
               (LU, r) -> corner nextRD LU - r - 1
               (UR, 0) -> corner nextRD LU - (side nextRD) + 1
               (UR, r) -> i + 1
  | d == R = case icorn of
               (RD, r) -> i + 1
               (DL, 0) -> i + 1
               (DL, r) -> corner prevRD DL - r + 1
               (LU, r) -> i - 1 
               (UR, r) -> corner nextRD UR - r - 1
  | d == DL = getNeighbor (getNeighbor i L) D
  | d == LU = getNeighbor (getNeighbor i U) L
  | d == UR = getNeighbor (getNeighbor i U) R
  | d == RD = getNeighbor (getNeighbor i D) R
  | otherwise = 0
  where
    icorn = distfromcorner i
    nxtsq = (rdCornerSquareroot i)+2
    nextRD = nxtsq^2
    prevsq = (rdCornerSquareroot i)-2
    prevRD = prevsq^2
    firstinsq = prevRD + 1


directions = [D,L,U,R]
corners = [RD,DL,LU,UR]
distfromcorner i = minimumBy (\d1 d2 -> compare (snd d1) (snd d2)) $
    filter (\(c,d) -> d >= 0 ) $ map (\d -> (d, corner i d - i)) corners
distfromcenter i = 
  max ((quot (side i) 2) - snd(distfromcorner i)) 
    (snd(distfromcorner i) - quot (side i) 2)

solution :: IO ()
solution = do
  putStrLn "bottom right corner"
  putStrLn $ show $ corner input RD 
  putStrLn "radius"
  putStrLn $ show $ radius input
  putStrLn "perimeter"
  putStrLn $ show $ perimeter (radius input)
  putStrLn "side"
  putStrLn $ show $ side input
  let result i =  distfromcenter i + (radius i)
  {-let result1 = (distfromcorner input) - quot (side input) 2 + radius input-}
  let result1 = distfromcorner input
  putStrLn $ show input
  putStrLn "Part I"
  putStrLn $ show $ result input

  putStrLn "--------- "
  putStrLn "Part II"

  let getsums :: Int -> Int
      getsums i = 
        let sumworker :: Int -> IM.IntMap Int -> Int
            sumworker j sums = 
               if currentsum > i
                  then currentsum
                  else let res = foldl
                               (\ac n -> case IM.lookup n sums of
                                             Nothing -> ac 
                                             Just v -> ac + v
                                 ) 0 
                                 (map (getNeighbor j) (corners ++ directions))
                           newsums = IM.insert j res sums
                        in sumworker (j+1) newsums
               where 
                 currentsum = fromJust $ IM.lookup (j-1) sums
         in sumworker 2 (IM.singleton 1 1)

  putStrLn "sum of "
  putStrLn $ show input
  putStrLn $ show $ getsums input
