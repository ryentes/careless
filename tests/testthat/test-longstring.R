context("test-longstring.R")

fname <- system.file("testdata", "lonstring-test-data.csv", package="careless")
ls.test.data <- read.csv(fname, header=T)

test_that("basic longstring output is correct", {
  expect_identical(longstring(ls.test.data), c(1:9*10,10))
})
