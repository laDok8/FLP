module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import System.IO

import Types
import ParseInput
import Minimize

main = do
    -- TODO args parse
    putStrLn "What's your first name?"
    firstName <- getLine
    let bigFirstName = map toUpper firstName
    putStrLn $ "hey " ++ bigFirstName ++ ", sup"
    withFile "test/test01.in" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStrLn contents
            let inputSack = getKnapsackProblem contents
            putStrLn $ show inputSack
            outputSack <- solveKnapSack (SackM 0 0 []) inputSack
            print outputSack
            )
