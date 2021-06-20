#' Computes the psychometric synonym/antonym score
#'
#' Takes a matrix of item responses and identifies item pairs that are highly
#' correlated within the overall dataset.
#' What defines "highly correlated" is set by the critical value (e.g., r > .60).
#' Each respondents' psychometric synonym
#' score is then computed as the within-person correlation between the
#' identified item-pairs.
#' Alternatively computes the psychometric antonym score which is a
#' variant that uses item pairs that are highly \emph{negatively} correlated.
#'
#' @param x is a matrix of item responses
#' @param critval is the minimum magnitude of the correlation between two items
#' in order for them to be considered psychometric synonyms. Defaults to .60
#' @param anto determines whether psychometric antonyms are returned instead of
#' psychometric synonyms. Defaults to \code{FALSE}
#' @param n_pairs additionally return the number of item pairs available for each observation. Useful if dataset contains many missing values.
#' @param resample_na if psychsyn returns NA for a respondent resample to attempt getting a non-NA result.
#' @param diag  deprecated. use n_pairs instead.
#' @author Richard Yentes \email{ryentes@gmail.com}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#' Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data.
#' \emph{Psychological Methods, 17(3)}, 437-455. \doi{10.1037/a0028085}
#' @seealso \code{\link{psychant}} for a more concise way to calculate the psychometric antonym score,
#'  \code{\link{psychsyn_critval}} for a helper that allows to set an
#' adequate critical value for the size of the correlation.
#' @export
#' @examples
#' synonyms <- psychsyn(careless_dataset, .60)
#' antonyms <- psychsyn(careless_dataset2, .50, anto = TRUE)
#' antonyms <- psychant(careless_dataset2, .50)
#'
#' #with diagnostics
#' synonyms <- psychsyn(careless_dataset, .60, n_pairs = TRUE)
#' antonyms <- psychant(careless_dataset2, .50, n_pairs = TRUE)

psychsyn <- function(x, critval=.60, anto=FALSE, n_pairs=FALSE, resample_na=TRUE, diag=NULL) {
  x <- as.matrix(x) #Comment: why this?

  if(!missing(diag)) {
    warning("diag argument has been renamed to n_pairs and has been deprecated as of version x.x.x, please use the n_pairs argument instead.")
    n_pairs <- diag
  }
  # Helper function that identifies psychometric synonyms in a given dataset
  get_item_pairs <- function(x, critval=.60, anto=FALSE) {
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
  # get the item pairs above (below for anto = TRUE) the threshold
  item_pairs <- get_item_pairs(x, critval, anto)

  # Helper function to calculate the within person correlation for a single individual
  syn_for_one <- function(x, item_pairs, resample_na) {
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

  # calculate the within-person synonym cor for each person
  synonyms <- apply(x,1,syn_for_one, item_pairs, resample_na)
  synonyms_df <- as.data.frame(t(synonyms)) # recast as data.frame with one row per observation
  colnames(synonyms_df) <- c("numPairs", "cor")
  # return number of available pairs if requested
  if(n_pairs==TRUE) { return(synonyms_df) }
  else { return(synonyms_df$cor) }
}


