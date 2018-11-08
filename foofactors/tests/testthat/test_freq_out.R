context("frequency table for a factor")

test_that("expectation for success", {
  expect_equal(freq_out(factor(mtcars$cyl))$n, c(11,7,14))
  expect_equal(freq_out(factor(iris$Species))$n, c(50,50,50))
})
