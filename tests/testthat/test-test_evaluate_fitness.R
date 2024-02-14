test_that("evaluate_fitness returns correct fitness values", {
    fitness <- genetic.algo.optimizeR::evaluate_fitness(c(1, 3, 0))
    expect_equal(fitness, c(1, 1, 4))
})
