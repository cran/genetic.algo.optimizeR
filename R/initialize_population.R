#' Initialize population
#'
#' The function described below creates a population of individuals(values).
#'
#' (Note: the values are random and the population should be highly diversified) \cr
#' The space of x value is kept integer type and on range from 0 to 3, for simplification.
#'
#' @param population_size The number of individuals of the population.
#' @param min The minimum number an individual could have.
#' @param max The maximum number an individual could have.
#'
#' @returns The output expected should be a list of individuals of the population
#' with the size indicated in the input.
#'
#' @examples
#' # example of usage
#' genetic.algo.optimizeR::initialize_population(population_size = 3, min = 0, max = 3)
#'
#' @author Dany Mukesha
#'
#' @export
initialize_population <- function(population_size, min = -100, max = 100) {
    large_population <-
        runif(10 * population_size, min = min, max = max) |>
        round(x = _, digits = 0)

    if (abs(min - max) < population_size) {
        population <- large_population |>
            unique() |>
            sample(size = population_size, replace = TRUE)
    } else {
        population <- large_population |>
            unique() |>
            sample(size = population_size)
    }

    return(population)
}
