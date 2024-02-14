context("Testing crossover function")

test_that("crossover generates correct number of offspring", {
    selected_parents <- c(1, 3)
    offspring <- genetic.algo.optimizeR::crossover(selected_parents, offspring_size = 2)
    expect_equal(length(offspring), 2)
})

# test_that("crossover generates offspring with correct values", {
#   selected_parents <- c(1, 3)
#   offspring <- genetic.algo.optimizeR::crossover(selected_parents, offspring_size = 2)
#   expect_equal(sum(offspring), sum(selected_parents)) # Sum of offspring should be the sum of selected parents
# })
