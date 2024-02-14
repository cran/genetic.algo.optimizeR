test_that("replacement replaces correct number of individuals", {
    population <- c(1, 3, 0)
    offspring <- c(2, 2)
    replaced_population <- genetic.algo.optimizeR::replacement(population, offspring, num_to_replace = 1)
    expect_equal(length(unique(replaced_population)), length(population)) # Check if all individuals are unique
})
