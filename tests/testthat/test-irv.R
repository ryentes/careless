manual_irv_calc <- function(j) {
  mean <- mean(j, na.rm = TRUE)
  k <- length(j)-sum(is.na(j)) # adjusting for missing values
  enumerator <- sum((j-mean)^2, na.rm = TRUE)
  denominator <- k-1
  sqrt(enumerator/denominator)
}

# example testdata taken from Curran 2016
testdata <- matrix(c(rep(3,10), c(1,2,3,4,5,1,2,3,4,5), c(1,1,1,1,1,5,5,5,5,5)),
                     nrow =3, ncol = 10, byrow = TRUE)

out <- apply(testdata, 1, manual_irv_calc)

test_that("basic IRV is correct", {
  expect_identical(irv(testdata), out)
})


# test splits
data_split <- careless_dataset[1:2,1:30]
irv_out <- irv(data_split, split = TRUE)

test_that("IRV split", {
  expect_equal(irv_out$irv1[1], manual_irv_calc(as.numeric(data_split[1,1:10])))
  expect_equal(irv_out$irv2[1], manual_irv_calc(as.numeric(data_split[1,11:20])))
  expect_equal(irv_out$irv3[1], manual_irv_calc(as.numeric(data_split[1,21:30])))
})

# NA handling
data_na <- careless_dataset[1:2,1:30]
data_na[2,] <- NA
data_na[1,3] <- NA

irv_out <- irv(data_na, split = TRUE)

test_that("IRV NA handling", {
  expect_equal(irv_out$irvTotal[2], NA_real_)
  expect_equal(irv_out$irvTotal[1], manual_irv_calc(as.numeric(data_na[1,])))
})



