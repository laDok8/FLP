module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char

import Types
import ParseInput


main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
    putStrLn $ getInput "proj1/test/test01.in"






its :: [(Int,Int)]
its = [(10,60),(20,100),(30,120)]


its2 :: [(Int,Int)]
its2 = [(4,1),(5,2),(1,3)]

singleton :: Sack
singleton = Sack 0 0 []

solveKnapSack :: Sack -> Int -> [(Int,Int)] -> Sack
solveKnapSack (Sack weight cost its) _ [] = (Sack weight cost its)
solveKnapSack (Sack weight cost its) maxW (x:xs)
    | (fst x) + weight > maxW = solveKnapSack (sackInsert (Sack weight cost its) (-1) (-1)) maxW xs
    | otherwise = max
                    (solveKnapSack (sackInsert (Sack weight cost its) (fst x) (snd x)) maxW xs)
                    (solveKnapSack (sackInsert (Sack weight cost its) (-1) (-1)) maxW xs)
