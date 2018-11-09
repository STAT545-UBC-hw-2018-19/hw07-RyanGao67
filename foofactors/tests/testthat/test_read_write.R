context("Check write/read data frame")

# test case to check if we manually create a data frame
test_that("Manually create a data frame", {
  df <- data.frame(
    x = 1,
    y = 1:10,
    fac = sample(LETTERS[1:3], 10, replace = TRUE)
  )

  dfwrite(df, "./test_dfwrite_dfread/test_manual.csv", "./test_dfwrite_dfread/levels_manual.txt")
  test_df <- dfread("./test_dfwrite_dfread/test_manual.csv", "./test_dfwrite_dfread/levels_manual.txt")

  expect_equal(levels(test_df$fac), levels(df$fac))
})

# test case to report an error
test_that("Not a data frame", {
  f <- factor(c("s", "t", "a"))

  expect_error(dfwrite(f))
  expect_error(dfread(f))
})

# test case if we reorder a factor of the data frame
test_that("Reorder factor of a data frame", {
  df <- data.frame(
    x = 1,
    y = 1:10,
    fac = sample(LETTERS[1:3], 10, replace = TRUE)
  )

  df$fac <- newreorder(df$fac)

  dfwrite(df, "./test_dfwrite_dfread/test_reorder.csv", "./test_dfwrite_dfread/levels_reorder.txt")
  test_df <- dfread("./test_dfwrite_dfread/test_reorder.csv", "./test_dfwrite_dfread/levels_reorder.txt")

  expect_equal(levels(test_df$fac), levels(df$fac))
})


