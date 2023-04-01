{-
FLP Project 1: Knapsack Problem
Author: Ladislav Dokoupil
Login: xdokou14
Year: 2023
-}

module Minimize
( solveKnapSackBrute
, solveKnapSackGA
) where

import qualified Data.List as List
import System.Random
import Types

--interface for brute force solution
solveKnapSackBrute :: SackInput -> [Int]
solveKnapSackBrute inps@(SackInput maxW minC items) = let
    solved = solveKnapSack inps [] 0
    in if (fitness items solved maxW) < minC then [] else solved

--brute force solution
solveKnapSack :: SackInput -> [Int] -> Int -> [Int]
solveKnapSack (SackInput _ _ []) curItems _ = curItems
solveKnapSack (SackInput maxW minC (x:xs)) curItems cur_weight
    | (weight x)  + cur_weight > maxW = solveKnapSack (SackInput maxW minC xs) (curItems ++ [0]) cur_weight
    | otherwise = do
          let s1 = solveKnapSack (SackInput maxW minC xs) (curItems ++ [1]) (cur_weight + weight x)
          let s2 = solveKnapSack (SackInput maxW minC xs) (curItems ++ [0]) cur_weight
          if (sum s1) > (sum s2) then s1 else s2


--solution using genetic algorithm ↓↓

-- Create a random genome of given length
createGenome :: Int -> Int -> [Int]
createGenome len gen = take len $ randomRs (0, 1) (mkStdGen gen)

-- Create a population of genomes with a given length and size
createPopulation :: Int -> Int -> [[Int]]
createPopulation len size = map (\g -> createGenome len g) [1..size]


-- Calculate the fitness of a genome
fitness :: (Eq a, Num a) => [SackItem] -> [a] -> Int -> Int
fitness items genome maximWeight =
  let (totalWeight, totalCost) = foldl (\(w, c) (SackItem w' c', g) -> if g == 1 then (w+w', c+c') else (w, c)) (0, 0) (zip items genome)
  in if totalWeight > maximWeight then 0 else totalCost


-- Select two genomes from the population using roulette wheel selection
rouletteWheelSelection :: RandomGen a => [([Int], Int)] -> a -> ([Int], [Int],a)
rouletteWheelSelection populationWithFitness gen0 = let
    totalFitness = sum $ map snd populationWithFitness
    wheel = scanl (\(_, accFitness) (genome, curFitness) -> (genome, accFitness + curFitness)) ([], 0) populationWithFitness
    (parent1Index,gen1) = randomR (1, totalFitness) gen0
    (parent2Index,gen2) = randomR (1, totalFitness) gen1
    parent1 = fst $ head $ dropWhile (\(_, accFitness) -> accFitness < parent1Index) wheel
    parent2 = fst $ head $ dropWhile (\(_, accFitness) -> accFitness < parent2Index) wheel
    in (parent1, parent2, gen2)

-- Iterate over all bits and mutate them with a given probability
mutate ::RandomGen a => [Int] -> Float -> a -> ([Int],a)
mutate genome mutRate gen0 = let
    (mutatedGenome, gen2) = foldl (\(g, gen) bit -> let
        (mutationRate, gen1) = randomR (0, 1) gen
        in if mutationRate < mutRate then (g ++ [1 - bit], gen1) else (g ++ [bit], gen1)) ([], gen0) genome
    in (mutatedGenome, gen2)


-- Crossover two parents using single-point crossover
crossover ::RandomGen a => [Int] -> [Int] -> Float -> Float -> a -> [[Int]]
crossover parent1 parent2 crRate mrRate gen0 = let
    (crossoverPoint, gen1) = randomR (0, length parent1 - 1) gen0
    (before, after) = splitAt crossoverPoint parent1
    (before2, after2) = splitAt crossoverPoint parent2
    (crossoverRate, gen2) = randomR (0, 1) gen1
    child1 = before ++ after2
    child2 = before2 ++ after
    --mutate
    (mutatedChildren1, gen3) = mutate child1 mrRate gen2
    (mutatedChildren2, _) = mutate child2 mrRate gen3
    in if crossoverRate < crRate then [mutatedChildren1, mutatedChildren2] else [parent1, parent2]


-- Evolve the population for a given number of generations using cur generation as seed
evolvePopulation :: [SackItem] -> [[Int]] -> Int -> Int -> Int -> Float -> Float -> [[Int]]
evolvePopulation items population maximWeight generation eliteCount crossoverRate mutationRate = let
    populationWithFitness = map (\g -> (g, fitness items g maximWeight)) population
    sortedPopulation = List.sortBy (\(_, f1) (_, f2) -> compare f2 f1) populationWithFitness
    elite = take eliteCount $ map fst sortedPopulation
    poolSize = ((length population - eliteCount) `div` 2 ) - 1
    parents = map (\g -> rouletteWheelSelection populationWithFitness (mkStdGen g)) [generation..(poolSize+generation)]
    children = concatMap (\(p1, p2, generator) -> crossover p1 p2 crossoverRate mutationRate generator) parents
    newPopulation = elite ++ children
    in if generation == 0 then newPopulation else evolvePopulation items newPopulation maximWeight (generation - 1) eliteCount crossoverRate mutationRate


-- Main function
solveKnapSackGA :: SackInput -> [Int]
solveKnapSackGA (SackInput maximWeight minimalCost items) = let
    populationSize = 30 -- must be div 2
    generations = 100
    elitismRate = 0.1 :: Float
    crossoverRate = 0.8 :: Float
    mutationRate = 0.1 :: Float
    eliteCount = floor (elitismRate * fromIntegral populationSize) `div` 2 * 2 --even
    genomeSize = length items
    initialPopulation = createPopulation genomeSize populationSize
    evolved = evolvePopulation items initialPopulation maximWeight generations eliteCount crossoverRate mutationRate
    newFitness = map (\g -> (g, fitness items g maximWeight)) evolved
    (champion,champCost) = List.maximumBy (\(_, f1) (_, f2) -> compare f1 f2) newFitness
    in if champCost < minimalCost then [] else champion
