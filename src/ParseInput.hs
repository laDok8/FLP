module ParseInput
 ( getInput
 ) where

import Types
import Text.Parsec

--eol :: Parsec String () Char
eol = do
    char '\n'
    spaces

-- Parsec parsers for the data types
knapsackParser :: Parsec String () SackInput
knapsackParser = do
  string "Knapsack {"
  eol
  maxWeight <- parseMaxWeight
  minCost <- parseMinCost
  items <- parseItems
  eol
  string "}"
  eol
  return (SackInput maxWeight minCost items)

itemParser :: Parsec String () SackItem
itemParser = do
  spaces
  string "Item {"
  eol
  weight <- parseWeight
  cost <- parseCost
  string "}"
  return (SackItem weight 1)

-- Helper parsers for parsing individual fields
parseMaxWeight :: Parsec String () Int
parseMaxWeight = do
  string "maxWeight:"
  spaces
  number <- many1 digit
  spaces
  return (read number)

parseMinCost :: Parsec String () Int
parseMinCost = do
  string "minCost:"
  spaces
  number <- many1 digit
  spaces
  return (read number)

parseWeight :: Parsec String () Int
parseWeight = do
  spaces
  string "weight:"
  spaces
  number <- many1 digit
  spaces
  return (read number)

parseCost :: Parsec String () Int
parseCost = do
  string "cost:"
  spaces
  number <- many1 digit
  spaces
  return (read number)

-- Parsec parser for the items list
parseItems = do
  string "items: ["
  items <- itemParser `sepBy` (char '\n' >> spaces)
  string "]"
  return items

-- Parsec parser for the entire input
getKnapsackProblem :: String -> SackInput
getKnapsackProblem input = case parse knapsackParser "" input of
  Left err -> error (show err)
  Right val -> val
