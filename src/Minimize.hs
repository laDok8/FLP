module Minimize
( solveKnapSack1
, solveKnapSack
) where

--imports
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import Types


--solveKnapSack1 :: SackInput -> SackM
solveKnapSack1 :: IO Int
solveKnapSack1 = return 1 --solveKnapSack (SackM 0 0 []) 42 [(4,1),(5,2),(1,3)]


--brute force solution
solveKnapSack :: SackM -> Int -> [(Int,Int)] -> SackM
solveKnapSack (SackM weight cost its) _ [] = (SackM weight cost its)
solveKnapSack (SackM weight cost its) maxW (x:xs)
    | (fst x) + weight > maxW = solveKnapSack (sackInsert (SackM weight cost its) (-1) (-1)) maxW xs
    | otherwise = max
                    (solveKnapSack (sackInsert (SackM weight cost its) (fst x) (snd x)) maxW xs)
                    (solveKnapSack (sackInsert (SackM weight cost its) (-1) (-1)) maxW xs)
