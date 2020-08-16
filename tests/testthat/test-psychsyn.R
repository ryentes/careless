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