module Minimize
( solveKnapSack
) where

--imports
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import Types


--brute force solution
solveKnapSack :: SackM -> SackInput -> IO SackM
solveKnapSack (SackM wg ct its) (SackInput _ _ []) = return (SackM wg ct its)
solveKnapSack (SackM wg ct its) (SackInput maxW minC (x:xs))
    | (weight x) + wg > maxW = solveKnapSack (SackM wg ct (its ++ [0])) (SackInput maxW minC xs)
    | otherwise = do
          s1 <- solveKnapSack (sackInsert (SackM wg ct its) (weight x) (cost x)) (SackInput maxW minC xs)
          s2 <- solveKnapSack (SackM wg ct (its ++ [0])) (SackInput maxW minC xs)
          return $ max s1 s2
