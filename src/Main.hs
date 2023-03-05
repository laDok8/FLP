module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import System.IO

import Types
import ParseInput
import Minimize




main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    let bigFirstName = map toUpper firstName
    putStrLn $ "hey " ++ bigFirstName ++ ", sup"
    withFile "test/test01.in" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStrLn contents
            solveKnapSack1
            )
    -- delete example
    --its :: [(Int,Int)]
    let its = [(10,60),(20,100),(30,120)]
    --its2 :: [(Int,Int)]
    let its2 = [(4,1),(5,2),(1,3)]
    --singleton :: SackM
    let singleton = SackM 0 0 []
    --solveKnapSack singleton 46 its2
    putStrLn $ show $ singleton



str = "Knapsack {\nmaxWeight: 46\nminCost: 324\nitems: [\n    Item {\n    weight: 36\n    cost: 3\n    }\n      Item {\n    weight: 36\n    cost: 3\n    }\n]\n}\n"
