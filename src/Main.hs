{-
FLP Project 1: Knapsack Problem
Author: Ladislav Dokoupil
Login: xdokou14
Year: 2023
-}

module Main where

import qualified Data.List as List
import System.Environment
import System.IO
import System.Exit
import Control.Exception

import ParseInput
import Minimize

main :: IO ()
main = do
    args <- getArgs

    --get file/stdin content
    hInput <- if (args == []) || (List.isPrefixOf "-" $ last args) then return stdin else do
        let inputFileName = last args
        openFile inputFileName ReadMode `catch` (\e -> do
            let err = show (e :: IOError)
            hPutStrLn stderr $ "Unable to open file" ++ inputFileName ++ "\nError: " ++ err
            exitWith $ ExitFailure 1)
    contents <- hGetContents hInput

    --parse input
    let inputSack = getKnapsackProblem contents

    --solve
    if "-i" `elem` args then do
        putStrLn $ show inputSack
    else if "-b" `elem` args then do
        outputSack <- return $ solveKnapSackBrute inputSack
        putStrLn $ show outputSack
    else if "-o" `elem` args then do
        outputSack <- return $ solveKnapSackGA inputSack
        putStrLn $ show outputSack
    else do
        putStrLn "No option specified"

    hClose hInput
