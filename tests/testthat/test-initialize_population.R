test_that("initialize_population generates correct number of individuals", {
    population <- genetic.algo.optimizeR::initialize_population(population_size = 10, min = 0, max = 3)
    expect_equal(length(population), 10)
})

test_that("initialize_population generates integers within specified range", {
    population <- genetic.algo.optimizeR::initialize_population(population_size = 10, min = 0, max = 3)
    expect_true(all(population >= 0 & population <= 3))
})
