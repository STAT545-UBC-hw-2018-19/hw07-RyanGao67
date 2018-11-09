
# The basic idea here is that I first introduce a data frame
# Then I reorder the data
# Then I write the data frame to a file
# Then I read the data from the file
# Then I compare the data that I created and the data that I fetched from the file

test_that("Reorder factor of a data frame", {
  # first create the dataframe
  dataframe <- data.frame(
    first = 1,
    second = 1:100,
    factor = sample(LETTERS[1:6], 10, replace = TRUE)
  )

  dataframe$factor <- newreorder(dataframe$factor)

  dfwrite(dataframe, "./test_reorder.csv", "./levels_reorder.txt")
  fetched <- dfread("./test_reorder.csv", "./levels_reorder.txt")

  expect_equal(levels(fetched$factor), levels(dataframe$factor))
})


