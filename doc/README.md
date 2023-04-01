# Functional and Logic Programming project
___

year: 2023

Author : Ladislav Dokoupil, xdokou14

## Description
Haskell Knapsack problem solver using genetic algorithm.

## Usage
```
$ ./proj1 [options] <input_file>
```
or using stdin

```
options:
    -i print parsed input
    -b print solution using brute force
    -o print solution using genetic algorithm
```

## input file format
```
Knapsack {
maxWeight: 46
minCost: 324
items: [
    Item {
    weight: 36
    cost: 3
    }
    .
    .
    .
]
}
