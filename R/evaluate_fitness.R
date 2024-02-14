#' Evaluating the fitness  of the population
#'
#' The function described below applies this equation: \deqn{f(x) = x^2 - 4x + 4}
#' It assesses every individual \eqn{x} of a given population,
#' to then provide an overview of how that population fits the equation.
#'
#' @param population The list of individuals of the population.
#'
#' @returns The output expected should be a list of \eqn{f(x)} values
#' calculated from individuals in the population.
#'
#' @examples
#' # example of usage
#' population <- c(1, 3, 0)
#' # Evaluate fitness
#' genetic.algo.optimizeR::evaluate_fitness(population)
#'
#' @author Dany Mukesha
#'
#' @export
evaluate_fitness <- function(population) {
    fitness <- population^2 - 4 * population + 4
    return(fitness)
}
