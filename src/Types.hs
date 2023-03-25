module Types
( SackItem(..)
, SackInput(..)
) where

-- input data
data SackItem = SackItem { weight :: Int, cost :: Int } deriving (Eq, Ord)
data SackInput = SackInput { maxWeight :: Int, minCost :: Int, its :: [SackItem] } deriving (Eq, Ord)

instance Show SackItem where
    show (SackItem w c) = "Item { weight: " ++ show w ++ ", cost: " ++ show c ++ " }"

instance Show SackInput where
    show (SackInput mw mc items) = "Knapsack { maxWeight: " ++ show mw ++ ", minCost: " ++ show mc ++ ", items: [ " ++ showItems items ++ " ] }"
        where
            showItems [] = ""
            showItems (x:xs) = show x ++ ", " ++ showItems xs
