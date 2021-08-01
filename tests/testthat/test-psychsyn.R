# test 1: calculate psych syn on a dataset with missings

# first, create a dataset with missings
dataset_na <- careless_dataset
replacements <- 500
random_row <- sample(1:nrow(dataset_na), replacements, replace = TRUE)
random_col <- sample(1:ncol(dataset_na), replacements, replace = TRUE)

for(i in 1:replacements) {
  dataset_na[random_row[i], random_col[i]] <- NA
}

synonyms <- psychsyn(dataset_na, .60)

test_that("All values within -1 to 1", {
  expect_lte(max(synonyms, na.rm = TRUE), 1)
  expect_gte(min(synonyms, na.rm = TRUE), -1)
})


synonyms_pairs <- psychsyn(dataset_na, .60, n_pairs = TRUE)
# antonym_pairs <- psychant(dataset_na, .15, n_pairs = TRUE) #no antonyms in dataset
synonyms <- psychsyn(dataset_na, .60, diag = TRUE)


# what if persons pass an unnamed matrix instead of a dataframe
dataset_na_matrix <- as.matrix(dataset_na)
colnames(dataset_na_matrix) <- NULL
synonyms_mat <- psychsyn(dataset_na_matrix, .60)

test_that("Input can be either dataframe or matrix", {
  expect_equal(synonyms$cor, synonyms_mat)
})

# manual test case
set.seed(1337)
generate_synonyms <- function(n) {
  y1 <- rnorm(n)
  y2 <- y1+rnorm(n, 0, 1)
  return(data.frame(y1, y2))
}

psychsyn_testdf <- cbind(generate_synonyms(100),
      generate_synonyms(100),
      generate_synonyms(100),
      generate_synonyms(100))
names(psychsyn_testdf) <- c("a1", "a2", "b1", "b2", "c1", "c2", "d1", "d2")
# cor(psychsyn_testdf)

psychsyn_testdf[101,] <- c(rep(5,8)) # 101 should be NA because it is uniform = no correlation possible
psychsyn_testdf[102,] <- c(4,5, # should have very high correlation
                           1,2,
                           8,9,
                           13,12)
psychsyn_testdf[103,] <- c(0, 6, # 103 has obverse function, should have -1 (but resampling may change that a bit)
                           1, 5,
                           2, 4,
                           3, 3)

psychsyn_testdf[104,] <- c(1, 5, # 103 has random allocation, should have 0
                           2, 3,
                           3, 3,
                           4, 5)

synonyms_2 <- psychsyn(psychsyn_testdf, .50)

test_that("Synonyms is within the expected range", {
  expect_gte(mean(synonyms_2, na.rm = TRUE), .40)
  expect_lte(mean(synonyms_2, na.rm = TRUE), .80)
})

test_that("Specific cases of synonyms", {
  expect_equal(synonyms_2[101], NA_integer_)
  expect_gte(synonyms_2[102], .85)
  expect_lte(synonyms_2[103], -.85)
  expect_equal(synonyms_2[104], 0)
})
