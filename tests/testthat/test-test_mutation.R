test_that("mutation correctly mutates offspring", {
    offspring <- c(1, 3, 0)
    mutated_offspring <- genetic.algo.optimizeR::mutation(offspring, mutation_rate = 1) # All offspring will be mutated
    expect_true(any(mutated_offspring != offspring))
})
