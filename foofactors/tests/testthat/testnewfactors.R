
test_that("in original order", {

  # check if function newfactor return the correct order
  a <- c("s","t","a","z")
  b <- c("this","is","a","test")
  c <- c("1","2","3","4")

  #Set levels of a factor to the order in which they appear in the data.
  expect_equal(levels(newfactor(factor(a))), a)
  expect_equal(levels(newfactor(factor(b))), b)
  expect_equal(levels(newfactor(factor(c))), c)
})

test_that("in reversed order", {

  # check if function newrev return the correct order
  a <- c("s","t","a","z")
  b <- c("this","is","a","test")
  c <- c("1","2","3","4")

  #Set levels of a factor to the reversed order in which they appear in the data.
  expect_equal(levels(newrev(factor(a))), c("z","a","t","s"))
  expect_equal(levels(newrev(factor(b))), c("test","a","is","this"))
  expect_equal(levels(newrev(factor(c))), c("4","3","2","1"))

})
