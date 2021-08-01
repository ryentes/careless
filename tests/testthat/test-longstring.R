basic = as.data.frame(t(matrix(rep(seq(10), 10), ncol = 10, nrow = 10)))
basic[lower.tri(basic, diag=TRUE)] = 1

# Test that the longstring function returns as expected on a basic dataset.
test_that("basic longstring output is correct", {
  expect_identical(longstring(basic), as.numeric(seq(10)))
})


# Test that longstring handles NA Values in the NA test dataset.
na_rows = c(103, 144, 138, 2, 194)
test_that("longstring runs correctly on NA values", {
  expect_identical(longstring(careless_testing_na[na_rows,]), c(1,4,5,4,3))
})