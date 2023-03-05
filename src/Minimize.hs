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
solveKnapSack1 :: IO SackM
solveKnapSack1 = solveKnapSack (SackM 0 0 []) 42 [(4,1),(5,2),(1,3)]


--brute force solution
solveKnapSack :: SackM -> Int -> [(Int,Int)] -> IO SackM
solveKnapSack (SackM weight cost its) _ [] = return (SackM weight cost its)
solveKnapSack (SackM weight cost its) maxW (x:xs)
    | (fst x) + weight > maxW = solveKnapSack (sackInsert (SackM weight cost its) (-1) (-1)) maxW xs
    | otherwise = do
              s1 <- solveKnapSack (sackInsert (SackM weight cost its) (fst x) (snd x)) maxW xs
              s2 <- solveKnapSack (sackInsert (SackM weight cost its) (-1) (-1)) maxW xs
              return $ max s1 s2
