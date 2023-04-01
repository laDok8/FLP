{-
FLP Project 1: Knapsack Problem
Author: Ladislav Dokoupil
Login: xdokou14
Year: 2023
-}

module ParseInput
 ( getKnapsackProblem
 ) where

import Types
import Text.Parsec

-- "macro" to eat spaces and newlines
eol :: Parsec String () ()
eol = do
    _ <- char '\n'
    spaces

-- root parser
knapsackParser :: Parsec String () SackInput
knapsackParser = do
  _ <- string "Knapsack {"
  eol
  maximWeight <- parseMaxWeight
  mCost <- parseMinCost
  items <- parseItems
  eol
  _ <- string "}"
  eol
  return (SackInput maximWeight mCost items)

itemParser :: Parsec String () SackItem
itemParser = do
    _ <- string "Item {"
    w <- parseWeight
    c <- parseCost
    eol
    _ <- string "}"
    eol
    return (SackItem w c)

parseMaxWeight :: Parsec String () Int
parseMaxWeight = do
  _ <- string "maxWeight:"
  spaces
  number <- many1 digit
  spaces
  return (read number)

parseMinCost :: Parsec String () Int
parseMinCost = do
  _ <- string "minCost:"
  spaces
  number <- many1 digit
  spaces
  return (read number)

parseWeight :: Parsec String () Int
parseWeight = do
  eol
  _ <- string "weight:"
  spaces
  number <- many1 digit
  return (read number)

parseCost :: Parsec String () Int
parseCost = do
  eol
  _ <- string "cost:"
  spaces
  number <- many1 digit
  return (read number)

parseItems :: Parsec String () [SackItem]
parseItems = do
  _ <- string "items: ["
  eol
  items <- many $ itemParser
  _ <- string "]"
  return items

-- interface
getKnapsackProblem :: String -> SackInput
getKnapsackProblem input = case parse knapsackParser "" input of
--print only string "fuck" if error no stacktrace
  Left err -> errorWithoutStackTrace $ "Unable to parse input\nError: " ++ show err
  Right val -> val
