module Minimize
( solveKnapSackBrute
, fitness
, createPopulation
, solveKnapSackGA
, rouletteWheelSelection
, mutate
, evolvePopulation
, inps
, evalFunction
) where

import qualified Data.Map as Map
import qualified Data.List as List
import System.Random
import Control.Monad (replicateM)
import Data.Char
import Types

--brute force solution for reference
solveKnapSackBrute inps = solveKnapSack inps [] 0

solveKnapSack :: SackInput -> [Int] -> Int -> [Int]
solveKnapSack (SackInput _ _ []) its cur_weight = its
solveKnapSack (SackInput maxW minC (x:xs)) its cur_weight
    | (weight x)  + cur_weight > maxW = solveKnapSack (SackInput maxW minC xs) (its ++ [0]) cur_weight
    | otherwise = do
          let s1 = solveKnapSack (SackInput maxW minC xs) (its ++ [1]) (cur_weight + weight x)
          let s2 = solveKnapSack (SackInput maxW minC xs) (its ++ [0]) cur_weight
          if (sum s1) > (sum s2) then s1 else s2


--solution using genetic algorithm

-- Create a random genome of given length
createGenome :: Int -> Int -> [Int]
createGenome len gen = take len $ randomRs (0, 1) (mkStdGen gen)

-- Create a population of genomes with a given length and size
createPopulation :: Int -> Int -> [[Int]]
createPopulation len size = map (\g -> createGenome len g) [1..size]


-- Calculate the fitness of a genome based on the total weight and cost of the items it includes
fitness :: (Eq a, Num a) => [SackItem] -> [a] -> Int -> Int
fitness items genome maxWeight =
  let (totalWeight, totalCost) = foldl (\(w, c) (SackItem w' c', g) -> if g == 1 then (w+w', c+c') else (w, c)) (0, 0) (zip items genome)
  in if totalWeight > maxWeight then 0 else totalCost

-- Crossover two parents using single-point crossover
crossover :: [a] -> [a] -> Int -> [[a]]
crossover parent1 parent2 gen = let
        index = fst $ randomR (0, length parent1 - 1) (mkStdGen gen)
        (c1_left, c1_right) = splitAt index parent1
        (c2_left, c2_right) = splitAt index parent2
        in [c1_left ++ c2_right, c2_left ++ c1_right]

-- Mutate a genome by flipping a random bit
mutate :: [Int] -> Int -> [Int]
mutate genome gen = let
        index = fst $ randomR (0, length genome - 1) (mkStdGen gen)
        (before, after) = splitAt index genome
        mutated = (head after + 1) `mod` 2 : tail after
    in before ++ mutated

-- Select two genomes from the population using roulette wheel selection
rouletteWheelSelection :: [([Int], Int)] -> Int -> ([Int], [Int])
rouletteWheelSelection populationWithFitness gen = let
    totalFitness = sum $ map snd populationWithFitness
    wheel = scanl (\(_, accFitness) (genome, fitness) -> (genome, accFitness + fitness)) ([], 0) populationWithFitness
    parent1Index = fst $ randomR (1, totalFitness) (mkStdGen gen)
    parent2Index = fst $ randomR (1, totalFitness) (mkStdGen $ gen+1)
    parent1 = fst $ head $ dropWhile (\(_, accFitness) -> accFitness < parent1Index) wheel
    parent2 = fst $ head $ dropWhile (\(_, accFitness) -> accFitness < parent2Index) wheel
    in (parent1, parent2)


-- TODO: iterate
-- Evolve the population for a given number of generations
--evolvePopulation :: [SackItem] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
evolvePopulation items population maxWeight maxGenerations eliteCount crossoverRate mutationRate = let
    genomeSize = length items
    populationWithFitness = map (\g -> (g, fitness items g maxWeight)) population
    sortedPopulation = List.sortBy (\(_, f1) (_, f2) -> compare f2 f1) populationWithFitness
    elite = take eliteCount $ map fst sortedPopulation
    parents = map (\g -> rouletteWheelSelection sortedPopulation g) [1..length sortedPopulation `div` 2]
    children = concatMap (\(p1, p2) -> crossover p1 p2 1) parents
    mutatedChildren = map (\g -> mutate g 2) children
    newPopulation = elite ++ mutatedChildren
    in if maxGenerations == 0 then newPopulation else evolvePopulation items newPopulation maxWeight (maxGenerations - 1) eliteCount crossoverRate mutationRate


-- TODO: iterating doesnt work prolly becase of the randoms
-- TODO: solve exceptions and mincost
-- The main function
solveKnapSackGA :: SackInput -> [Int]
solveKnapSackGA (SackInput maxWeight minCost items) = let
    populationSize = 4
    maxGenerations = 10
    elitismRate = 0.1
    crossoverRate = 0.8
    mutationRate = 0.1
    eliteCount = round $ fromIntegral populationSize * elitismRate
    genomeSize = length items
    initialPopulation = createPopulation genomeSize populationSize
    in head $ evolvePopulation items initialPopulation maxWeight maxGenerations eliteCount crossoverRate mutationRate

--mock usage TODO:delete
itss = [SackItem 1 1, SackItem 2 2, SackItem 3 3, SackItem 4 4, SackItem 5 5, SackItem 6 6, SackItem 7 7, SackItem 8 8, SackItem 9 9, SackItem 10 10]
inps = (SackInput 100 0 itss)
evalFunction its = fitness itss its 1000
