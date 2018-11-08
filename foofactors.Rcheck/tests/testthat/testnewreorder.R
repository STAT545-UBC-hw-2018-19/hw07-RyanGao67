context("my reorder")
# test the newreorder r script

test_that("expecataion for success", {
# first create the orginal factors

  a1 <- newreorder(factor(c("abandon","bbq","cook")))
  b1 <- newreorder(factor(c("1","2","3","4")))
  c1 <- newreorder(factor(c("a","s","t","z")))

# the following is the expected factors

  a2 <-c("cook","bbq","abandon")
  b2 <- c("4","3","2","1")
  c2 <-  c("z","t","s","a")

# Test if the computed and expected is equal

  expect_equal(levels(a1),a2)
  expect_equal(levels(b1),b2)
  expect_equal(levels(c1),c2)
})
