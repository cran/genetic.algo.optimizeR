#' Crossover of  selected parents from the fitting population
#'
#' This function performs crossover between the selected individuals that fit the best
#' based on the predefined condition(aim/objective). \cr
#' e.g.: To optimize the function \eqn{f(x) = x^2 - 4x + 4}
#' to find the value of \eqn{x} that minimizes the function.
#' \eqn{x}: represents a possible value the an individual from the population can have.
#'
#' @param selected_parents The list of selected individuals from the population.
#' @param offspring_size The number of offspring that the selected population should have.
#'
#' @returns The output expected should be a list of offspring for the next generation.
#'
#' @examples
#' population <- c(1, 3, 0)
#'
#' # Evaluate fitness
#' fitness <- evaluate_fitness(population)
#' print("Evaluation:")
#' print(fitness)
#'
#' # Selection
#' selected_parents <- selection(population, fitness, num_parents = 2)
#' print("Selection:")
#' print(selected_parents)
#'
#' # Crossover
#' offspring <- crossover(selected_parents, offspring_size = 2)
#' print("Crossover:")
#' print(offspring)
#'
#' @author Dany Mukesha
#'
#' @export
crossover <- function(selected_parents, offspring_size) {
    num_parents <- length(selected_parents)
    offspring <- numeric(offspring_size)

    for (i in seq(1, offspring_size, by = 2)) {
        parent1_index <- sample(seq_len(num_parents), 1, replace = TRUE)
        parent2_index <- sample(seq_len(num_parents), 1, replace = TRUE)

        offspring[i] <- (selected_parents[parent1_index] + selected_parents[parent2_index]) / 2
        offspring[i + 1] <- (selected_parents[parent1_index] + selected_parents[parent2_index]) / 2
    }
    offspring <- offspring |>
        round(x = _) # facilitate the adaptation of the offspring

    return(offspring)
}
