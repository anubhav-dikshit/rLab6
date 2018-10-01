context("test-rlab6_test.R")

test_that("brute_force_knapsack works", {
  expect_equal(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500), "$value 16770")
})
