context("Data exists")
library(salmer)

test_that("hymns has the right dimensions", {
  expect_equal(dim(hymns), c(24714, 9))
})
#> Test passed
