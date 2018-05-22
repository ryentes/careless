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
#' @param critVal is the minimum magnitude of the correlation between two items
#' in order for them to be considered psychometric synonyms. Defaults to .60
#' @param anto determines whether psychometric anonyms are returned instead of
#' psychometric synonyms. Defaults to \code{FALSE}
#' @param diag additionally return the number of item pairs available for each observation. Useful if dataset contains many missing values.
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#' Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data.
#' Psychological Methods, 17(3), 437-455. https://doi.org/10.1037/a0028085
#' @seealso \code{\link{psychant}} for a more concise way to calculate the psychometric antonym score,
#'  \code{\link{psychsynCritVal}} for a helper that allows to set an
#' adequate critical value for the size of the correlation.
#' @export
#' @examples
#' synonyms <- psychsyn(carelessDataset, .60)
#' antonyms <- psychsyn(carelessDataset2, .50, anto=TRUE)
#' antonyms <- psychant(carelessDataset2, .50)
#'
#' #with diagnostics
#' synonyms <- psychsyn(carelessDataset, .60, diag = TRUE)
#' antonyms <- psychant(carelessDataset2, .50, diag = TRUE)

psychsyn <- function(x, critVal=.60, anto=FALSE, diag=FALSE) {
  x <- as.matrix(x)
  itemPairs <- getItemPairs(x, critVal, anto)
  synonyms <- apply(x,1,synForOne, itemPairs)
  synonyms.df <- as.data.frame(aperm(synonyms))
  colnames(synonyms.df) <- c("numPairs", "cor")
  if(diag==TRUE) { return(synonyms.df) }
  else { return(synonyms.df$cor) }
}

# Helper function that identifies psychometric synonyms in a given dataset
getItemPairs <- function(x, critVal=.60, anto=FALSE) {
  x <- as.matrix(x)
  critVal <- abs(critVal) #Dummy Proofing

  correlations <- stats::cor(x, use = "pairwise.complete.obs")
  correlations[upper.tri(correlations, diag=TRUE)] <- NA
  correlations <- as.data.frame(as.table(correlations))

  # Identifying item pairs differs depending on whether the user wants
  # Psychometric Synonyms or Psychometric Antonyms
  if(anto==FALSE) {
    itemPairNames <- correlations[which(correlations$Freq > critVal, arr.ind=TRUE),c(1,2)]
    if(nrow(itemPairNames)==0) {
      stop("No Psychometric Synonyms found.")
    }
  }
  else if(anto==TRUE) {
    itemPairNames <- correlations[which(correlations$Freq < -critVal, arr.ind=TRUE),c(1,2)]
    if(nrow(itemPairNames)==0) {
      stop("No Psychometric Antonyms found.")
    }
  }

  matches <- itemPairNames
  return(matches)
}

# Helper function to calculate the within person correlation for a single individual
synForOne <- function(x, itemPairs) {
  itemPairs_omitNA <- which(!(is.na(x[itemPairs[,1]]) | is.na(x[itemPairs[,2]])))
  sumItemPairs <- length(itemPairs_omitNA)

  if(sumItemPairs > 2) {
      itemvalues <- cbind(as.numeric(x[as.numeric(itemPairs[,1])]), as.numeric(x[as.numeric(itemPairs[,2])]))
      synvalue <- suppressWarnings(stats::cor(itemvalues, use = "pairwise.complete.obs", method = "pearson")[1,2])

  } else {synvalue = NA}

  return(c(sumItemPairs, synvalue))
  }
