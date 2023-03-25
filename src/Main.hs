module Main where

import qualified Data.List as List
import System.IO
import System.Environment

import ParseInput
import Minimize

main :: IO ()
main = do
    args <- getArgs

    handle <- if (args == []) || (List.isPrefixOf "-" $ last args) then return stdin else do
        let inputFileName = last args
        openFile inputFileName ReadMode
    contents <- hGetContents handle

    let inputSack = getKnapsackProblem contents
    if "-i" `elem` args then do
        putStrLn $ show inputSack
    else if "-b" `elem` args then do
        outputSack <- return $ solveKnapSackBrute inputSack
        putStrLn $ if outputSack == [] then "False" else show outputSack
    else if "-o" `elem` args then do
        outputSack <- return $ solveKnapSackGA inputSack
        putStrLn $ if outputSack == [] then "False" else show outputSack
    else do
        putStrLn "No option specified"

    hClose handle
-- TODO: show and checkfile
