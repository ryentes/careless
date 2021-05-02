context("Mahalanobis distance")

# create mahaD results
mahad_psych <- psych::outlier(careless_dataset, plot= FALSE)
names(mahad_psych) <- NULL
mahad_stats <- stats::mahalanobis(careless_dataset,  colMeans(careless_dataset), var(careless_dataset))

test_that("Equality of mahalanobis distance for psych and stats package implementations", {
  expect_equal(mahad_psych, mahad_stats)
})

mahad_careless <- mahad(careless_dataset, plot = FALSE)

# with NA
careless_dataset_na <- careless_dataset
careless_dataset_na[c(5:8),] <- NA #entire observation NA
careless_dataset_na[1,3] <- NA #single value of obs is NA
mahad_careless_na <- mahad(careless_dataset_na, plot = FALSE)

mahad_psych_na <- psych::outlier(careless_dataset_na, plot= FALSE)
test_that("Equality of mahalanobis distance for careless and the other packages", {
  expect_equal(mahad_careless, mahad_stats)
  expect_equal(mahad_careless, mahad_psych)
  # expect_equal(mahad_careless_na, mahad_psych_na) # we assign NA, whereas psych assigns 0 to cases with complete NA
})

# flagging of outliers based on multivariate MahaD scores compared
# to expected scores given chi-square distribution
mahad_careless_flag <- mahad(careless_dataset, plot = FALSE, flag = TRUE, confidence = .95)
cut <- stats::qchisq(.95, ncol(careless_dataset)) # calculate chisquare with df = number of variables in datset, 95% quantile
flagged <- (mahad_careless_flag$d_sq > cut)

test_that("Outliers are flagged correctly based on chi-square", {
  expect_equal(mahad_careless_flag$flagged, flagged)
})
