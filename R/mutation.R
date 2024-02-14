#' Mutating the offspring
#'
#' The function described below mutates offspring from the selected individuals that fit the best
#' based on the predefined condition(aim/objective). \cr
#' e.g.: To optimize the function \eqn{f(x) = x^2 - 4x + 4}
#' to find the value of \eqn{x} that minimizes the function.
#' \eqn{x}: represents a possible value the an individual from the population can have.
#'
#' The mutation is needed to increase the diversity in the population
#' and help the next generation close to the fitness.
#'
#' @param offspring The list of offspring.
#' @param mutation_rate The probably of a single offspring to be modified/mutated.
#'
#' @returns The output expected should be a list of mutated offspring.
#'
#' @examples
#' # example of usage
#' population <- c(1, 3, 0)
#'
#' # Evaluate fitness.
#' fitness <- genetic.algo.optimizeR::evaluate_fitness(population)
#' print("Evaluation:")
#' print(fitness)
#'
#' # Selection
#' selected_parents <- genetic.algo.optimizeR::selection(population, fitness, num_parents = 2)
#' print("Selection:")
#' print(selected_parents)
#'
#' # Crossover
#' offspring <- genetic.algo.optimizeR::crossover(selected_parents, offspring_size = 2)
#' print("Crossover:")
#' print(offspring)
#'
#' # Mutation
#' mutated_offspring <- genetic.algo.optimizeR::mutation(offspring, mutation_rate = 0)
#' # (no mutation in this example)
#' print(mutated_offspring)
#'
#' @author Dany Mukesha
#'
#' @export
mutation <- function(offspring, mutation_rate) {
    num_offspring <- length(offspring)

    for (i in seq(num_offspring)) {
        if (runif(1) < mutation_rate) {
            offspring[i] <- offspring[i] + rnorm(1, mean = 0, sd = 0.1) |> # Add small random value
                round(x = _, digits = 1)
        }
    }

    return(offspring)
}
