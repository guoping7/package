context("Testing addFirstTwo function with numeric inputs")

test_that("Adding first two elements of numeric vector works correctly", {
  vec <- c(1, 2, 3)
  expect_equal(addFirstTwo(vec), 3)
})

test_that("Adding first two elements of non-numeric vector returns NA", {
  vec <- c("a", "b", "c")
  expect_equal(addFirstTwo(vec), NA)
})

test_that("Adding first two elements with missing values returns NA", {
  vec <- c(NA, 2, 3)
  expect_equal(addFirstTwo(vec), NA)
})
