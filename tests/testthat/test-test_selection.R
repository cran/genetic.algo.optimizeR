test_that("selection selects correct number of individuals", {
    population <- c(1, 3, 0)
    fitness <- genetic.algo.optimizeR::evaluate_fitness(population)
    selected_parents <- genetic.algo.optimizeR::selection(population, fitness, num_parents = 2)
    expect_equal(length(selected_parents), 2)
})
