## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
# devtools::install_github("danymukesha/genetic.algo.optimizeR", upgrade = c("never"),)
library(genetic.algo.optimizeR)

## -----------------------------------------------------------------------------
# Initialize population
population <- initialize_population(population_size = 3, min = 0, max = 3)
population

## -----------------------------------------------------------------------------
# Evaluate fitness
fitness <- evaluate_fitness(population)
fitness

## -----------------------------------------------------------------------------
# Perform selection
selected_parents <- selection(population, fitness, num_parents = 2)
selected_parents

## -----------------------------------------------------------------------------
# Perform crossover
offspring <- crossover(selected_parents, offspring_size = 2)
offspring

# Perform mutation
mutated_offspring <- mutation(offspring, mutation_rate = 0.1)
mutated_offspring

## -----------------------------------------------------------------------------
# Replace individuals in the population
new_population <- replacement(population, mutated_offspring, num_to_replace = 1)
new_population

## -----------------------------------------------------------------------------
# Termination
# Repeat the above steps(from Evaluation) for multiple generations or until a termination condition is met.

