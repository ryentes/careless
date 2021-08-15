psychsyn_legacy <- function(x, critval=.60, anto=FALSE, diag=FALSE, resample_na=TRUE) {
  x <- as.matrix(x)
  item_pairs <- get_item_pairs_legacy(x, critval, anto)

  synonyms <- apply(x,1,syn_for_one_legacy, item_pairs, resample_na)
  synonyms_df <- as.data.frame(aperm(synonyms))
  colnames(synonyms_df) <- c("numPairs", "cor")

  if(diag==TRUE) { return(synonyms_df) }
  else { return(synonyms_df$cor) }
}

# Helper function that identifies psychometric synonyms in a given dataset
get_item_pairs_legacy <- function(x, critval=.60, anto=FALSE) {
  critval <- abs(critval) #Dummy Proofing

  correlations <- stats::cor(x, use = "pairwise.complete.obs")
  correlations[upper.tri(correlations, diag=TRUE)] <- NA
  correlations <- as.data.frame(as.table(correlations))

  # Identifying item pairs differs depending on whether the user wants
  # Psychometric Synonyms or Psychometric Antonyms
  if(anto==FALSE) {
    item_pair_names <- correlations[which(correlations$Freq > critval, arr.ind=TRUE),c(1,2)]
    if(nrow(item_pair_names)==0) {
      stop("No Psychometric Synonyms found.")
    }
  }
  else if(anto==TRUE) {
    item_pair_names <- correlations[which(correlations$Freq < -critval, arr.ind=TRUE),c(1,2)]
    if(nrow(item_pair_names)==0) {
      stop("No Psychometric Antonyms found.")
    }
  }

  matches <- item_pair_names
  return(matches)
}

# Helper function to calculate the within person correlation for a single individual
syn_for_one_legacy <- function(x, item_pairs, resample_na) {
  item_pairs_omit_na <- which(!(is.na(x[item_pairs[,1]]) | is.na(x[item_pairs[,2]])))
  sum_item_pairs <- length(item_pairs_omit_na)
  #only execute if more than two item pairs
  if(sum_item_pairs > 2) {
    itemvalues <- cbind(as.numeric(x[as.numeric(item_pairs[,1])]), as.numeric(x[as.numeric(item_pairs[,2])]))

    # helper that calculates within-person correlation
    psychsyn_cor <- function(x) {
      suppressWarnings(stats::cor(x, use = "pairwise.complete.obs", method = "pearson")[1,2])
    }

    # if resample_na == TRUE, re-calculate psychsyn should a result return NA
    if(resample_na == TRUE) {
      counter <- 1
      synvalue <- psychsyn_cor(itemvalues)
      while(counter <= 10 & is.na(synvalue)) {
        itemvalues <- t(apply(itemvalues, 1, sample, 2, replace = F))
        synvalue <- psychsyn_cor(itemvalues)
        counter = counter+1
      }
    } else {
      synvalue <- psychsyn_cor(itemvalues) # executes if resample_na == FALSE
    }

  } else {synvalue <- NA} # executes if insufficient item pairs

  return(c(sum_item_pairs, synvalue))
}

synonyms <- psychsyn(careless_testing_na, .60, diag = TRUE)
synonyms2 <- psychsyn_legacy(careless_testing_na, .60, diag = TRUE)

test_that("Equality with diag", {
  expect_equal(synonyms$cor, synonyms2$cor)
})

synonyms <- psychsyn(careless_testing_na, .60)
synonyms2 <- psychsyn_legacy(careless_testing_na, .60)

test_that("Equality without diag", {
  expect_equal(synonyms, synonyms)
})

synonyms <- psychsyn(careless_dataset, .60)
synonyms2 <- psychsyn_legacy(careless_dataset, .60)

test_that("Equality in non-NA dataset", {
  expect_equal(synonyms, synonyms)
})
