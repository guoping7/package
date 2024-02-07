context("Testing addFirstTwo function with edge cases")

test_that("Adding first two elements with only one element returns NA", {
  vec <- c(1)
  expect_equal(addFirstTwo(vec), NA)
})

test_that("Adding first two elements with empty vector returns NA", {
  vec <- numeric(0)
  expect_equal(addFirstTwo(vec), NA)
})

test_that("Adding first two elements with non-numeric elements returns NA", {
  vec <- c("x", "y", "z")
  expect_equal(addFirstTwo(vec), NA)
})
