module ParseInput
 ( getKnapsackProblem
 ) where

import Types
import Text.Parsec

eol = do
    char '\n'
    spaces

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
    string "Item {"
    weight <- parseWeight
    cost <- parseCost
    eol
    string "}"
    eol
    return (SackItem weight cost)

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
  eol
  string "weight:"
  spaces
  number <- many1 digit
  return (read number)

parseCost :: Parsec String () Int
parseCost = do
  eol
  string "cost:"
  spaces
  number <- many1 digit
  return (read number)

parseItems :: Parsec String () [SackItem]
parseItems = do
  string "items: ["
  eol
  items <- many $ itemParser
  string "]"
  return items

-- interface
getKnapsackProblem :: String -> SackInput
getKnapsackProblem input = case parse knapsackParser "" input of
  Left err -> error (show err)
  Right val -> val
