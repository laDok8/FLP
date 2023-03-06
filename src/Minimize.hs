module Minimize
( solveKnapSack
, createGenome
) where

--imports
import qualified Data.Map as Map
import qualified Data.List as List
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Char
import Types


--brute force solution
solveKnapSack :: SackM -> SackInput -> IO SackM
solveKnapSack (SackM wg ct its) (SackInput _ _ []) = return (SackM wg ct its)
solveKnapSack (SackM wg ct its) (SackInput maxW minC (x:xs))
    | (weight x) + wg > maxW = solveKnapSack (SackM wg ct (its ++ [0])) (SackInput maxW minC xs)
    | otherwise = do
          s1 <- solveKnapSack (sackInsert (SackM wg ct its) (weight x) (cost x)) (SackInput maxW minC xs)
          s2 <- solveKnapSack (SackM wg ct (its ++ [0])) (SackInput maxW minC xs)
          return $ max s1 s2

--solution using genetic algorithm


-- Create a random genome (list of 0s and 1s) of a given length
createGenome :: Int -> IO [Int]
createGenome len = replicateM len $ randomRIO (0, 1)

-- Calculate the fitness of a genome based on the total weight and cost of the items it includes
calculateFitness :: [SackItem] -> [Int] -> Int -> Int -> Int
calculateFitness items genome maxWeight minCost =
  let (totalWeight, totalCost) = foldl (\(w, c) (SackItem w' c', g) -> if g == 1 then (w+w', c+c') else (w, c)) (0, 0) (zip items genome)
  in if totalWeight > maxWeight || totalCost < minCost then 0 else totalCost

-- Create a population of genomes with a given length and size
createPopulation :: Int -> Int -> IO [[Int]]
createPopulation len size = replicateM size (createGenome len)

-- Select two genomes from the population using roulette wheel selection
rouletteWheelSelection :: [([Int], Int)] -> IO ([Int], [Int])
rouletteWheelSelection populationWithFitness = do
  let totalFitness = sum $ map snd populationWithFitness
  let wheel = scanl (\(_, accFitness) (genome, fitness) -> (genome, accFitness + fitness)) ([], 0) populationWithFitness
  parent1Index <- randomRIO (1, totalFitness)
  parent2Index <- randomRIO (1, totalFitness)
  let parent1 = fst $ head $ dropWhile (\(_, accFitness) -> accFitness < parent1Index) wheel
  let parent2 = fst $ head $ dropWhile (\(_, accFitness) -> accFitness < parent2Index) wheel
  return (parent1, parent2)

-- Crossover two genomes using single-point crossover
crossover :: [Int] -> [Int] -> IO [Int]
crossover parent1 parent2 = do
  crossoverPoint <- randomRIO (0, length parent1 - 1)
  let child1 = take crossoverPoint parent1 ++ drop crossoverPoint parent2
  let child2 = take crossoverPoint parent2 ++ drop crossoverPoint parent1
  coin <- randomRIO (0, 1) :: IO Int
  return $ if coin == 0 then child1 else child2

-- Mutate a genome by flipping a random bit
mutate :: [Int] -> IO [Int]
mutate genome = do
  index <- randomRIO (0, length genome - 1)
  let (before, after) = splitAt index genome
  let mutated = (head after + 1) `mod` 2 : tail after
  return $ before ++ mutated


-- here lies the errors (mismatch type for population )
-- Evolve the population for a given number of generations
evolvePopulation :: [SackItem] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO [Int]
evolvePopulation items genomeSize populationSize maxWeight minCost maxGenerations elitismRate crossoverRate mutationRate = do
  initialPopulation <- createPopulation genomeSize populationSize
  let populationWithFitness = map (\g -> (g, calculateFitness items g maxWeight minCost)) initialPopulation
  evolvedPopulation <- evolvePopulation' items populationWithFitness maxWeight minCost maxGenerations elitismRate crossoverRate mutationRate
  let bestGenome = fst $ List.maximumBy (\(_, f1) (_, f2) -> compare f1 f2) evolvedPopulation
  return bestGenome

-- Evolve the population for a given number of generations
evolvePopulation' :: [SackItem] -> [([Int], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> IO [([Int], Int)]
evolvePopulation' items populationWithFitness maxWeight minCost maxGenerations elitismRate crossoverRate mutationRate =
  if maxGenerations == 0 then
    return populationWithFitness
  else do
    let sortedPopulation = List.sortBy (\(_, f1) (_, f2) -> compare f2 f1) populationWithFitness
    let eliteCount = round $ fromIntegral (length sortedPopulation) * fromIntegral elitismRate
    let elite = take eliteCount sortedPopulation
    let nonElite = drop eliteCount sortedPopulation
    parents <- replicateM (length nonElite `div` 2) (rouletteWheelSelection nonElite >>= crossoverParents)
    children <- mapM (uncurry crossover) parents
    mutatedChildren <- mapM mutate children
    let newPopulation = elite ++ map (\g -> (g, calculateFitness items g maxWeight minCost)) mutatedChildren
    evolvePopulation' items newPopulation maxWeight minCost (maxGenerations - 1) elitismRate crossoverRate mutationRate

-- Crossover two parents and return their offspring
crossoverParents :: ([Int], [Int]) -> IO ([Int], [Int])
crossoverParents (parent1, parent2) = do
  coin <- randomRIO (0, 1) :: IO Int
  if coin == 0
    then return (parent1, parent2)
    else return (parent2, parent1)

-- The main function
solveKnapSackGA :: SackInput -> IO [Int]
solveKnapSackGA (SackInput maxWeight minCost items) = do
  let genomeSize = length items
  let populationSize = 100
  let maxGenerations = 100
  let elitismRate = 0.1
  let crossoverRate = 0.8
  let mutationRate = 0.1
  result <- evolvePopulation items genomeSize populationSize maxWeight minCost maxGenerations elitismRate crossoverRate mutationRate
  return result



-- Here, we have added the function evolvePopulation' which is used to recursively evolve the population for the given number of generations.
-- The evolvePopulation' function first sorts the population based on the fitness of the genomes in descending order. It then selects the elite
--genomes (the top genomes with the best fitness) and the non-elite genomes for crossover and mutation.
--The function first selects parents using roulette wheel selection and then crosses them over using a single-point crossover. The function
--also adds some randomness by flipping a random bit during mutation.
--Finally, the function calls itself recursively with the new population until it has reached the desired number of generations. At the end of the process,
--it returns the population with the highest fitness value, which represents the best solution.
