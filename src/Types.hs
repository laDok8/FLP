module Types
( SackItem(..)
, SackInput(..)
) where

-- input data
data SackItem = SackItem { weight :: Int, cost :: Int } deriving (Eq, Show, Ord)
data SackInput = SackInput { maxWeight :: Int, minCost :: Int, its :: [SackItem] } deriving (Eq, Show, Ord)
