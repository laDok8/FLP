module Types
( SackM(..)
, SackItem(..)
, SackInput(..)
, sackInsert
) where

-- data structure for sack M for minimize
data SackM = SackM { c :: Int, w:: Int, items :: [Int] } deriving (Eq, Show, Ord)

-- 0,0 empty insert
sackInsert :: SackM -> Int -> Int -> SackM
sackInsert (SackM w c items) w' c'
    | all (>=0) [w',c'] = SackM (w+w') (c+c') (items ++ [1])
    | otherwise = SackM w c (items ++ [0])


-- input data
data SackItem = SackItem { weight :: Int, cost :: Int } deriving (Eq, Show, Ord)
data SackInput = SackInput { maxWeight :: Int, minCost :: Int, its :: [SackItem] } deriving (Eq, Show, Ord)
