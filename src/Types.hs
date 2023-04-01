{-
FLP Project 1: Knapsack Problem
Author: Ladislav Dokoupil
Login: xdokou14
Year: 2023
-}

module Types
( SackItem(..)
, SackInput(..)
, Solution(..)
) where

-- input file Item
data SackItem = SackItem { weight :: Int, cost :: Int } deriving (Eq, Ord)
-- parsed Input file
data SackInput = SackInput { maxWeight :: Int, minCost :: Int, its :: [SackItem] } deriving (Eq, Ord)

data Solution = Solution [Int] deriving (Eq, Ord)

instance Show SackItem where
    show (SackItem w c) = "Item { weight: " ++ show w ++ ", cost: " ++ show c ++ " }"

instance Show SackInput where
    show (SackInput mw mc items) = "Knapsack { maxWeight: " ++ show mw ++ ", minCost: " ++ show mc ++ ", items: [ " ++ showItems items ++ " ] }"
        where
            showItems [] = ""
            showItems (x:xs) = show x ++ ", " ++ showItems xs

instance Show Solution where
    show (Solution []) = "False"
    show (Solution s) = "Solution [" ++ showItems s ++ "]"
        where
            showItems [] = ""
            showItems (x:[]) = show x
            showItems (x:xs) = show x ++ " " ++ showItems xs
