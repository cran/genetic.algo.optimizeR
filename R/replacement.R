#' Replacing non-selected individual(s)
#'
#' This function replace the individual(s) that was/were not selected (i.e. not the best fit)
#' based on the predefined condition(aim/objective). \cr
#' e.g.: To optimize the function \eqn{f(x) = x^2 - 4x + 4}
#' to find the value of \eqn{x} that minimizes the function.
#' \eqn{x}: represents a possible value the an individual from the population can have.
#'
#' @param population The list of individuals of the population
#' @param offspring The list of offspring.
#' @param num_to_replace The number of selected individuals that should be replaced
#' in the population.
#'
#' @returns The output expected should be a list of selected individuals
#' that fit the best with the predefined aim.
#'
#' @examples
#' # example of usage
#' population <- c(1, 3, 0)
#'
#' # Evaluate fitness
#' fitness <- genetic.algo.optimizeR::evaluate_fitness(population)
#' print("Evaluation:")
#' print(fitness)
#'
#' # Selection
#' selected_parents <- genetic.algo.optimizeR::selection(population, fitness, num_parents = 2)
#' print("Selection:")
#' print(selected_parents)
#'
#' # Crossover and mutation
#' offspring <- genetic.algo.optimizeR::crossover(selected_parents, offspring_size = 2)
#' mutated_offspring <- mutation(offspring, mutation_rate = 0) # (no mutation in this example)
#' print(mutated_offspring)
#'
#' # Replacement
#' population <- genetic.algo.optimizeR::replacement(population, mutated_offspring, num_to_replace = 1)
#' print("Replacement:")
#' print(population)
#'
#' @author Dany Mukesha
#'
#' @export
replacement <- function(population, offspring, num_to_replace) {
    num_population <- length(population)
    unsurvivor <- setdiff(population, offspring)
    replace_indices <- sample(length(unsurvivor), num_to_replace, replace = FALSE)
    population[population %in% unsurvivor[replace_indices]] <- offspring[seq_len(num_to_replace)]
    return(population)
}
