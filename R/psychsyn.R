#' Computes the psychometric synonym score
#' 
#' Takes a matrix of item responses and identifies item pairs that are highly 
#' correlated within the overall dataset. Each respondents psychometric synonym
#' score is then comptued as the within-person correlation between the
#' identified item-pairs.
#' 
#' 
#' @param data is a matrix of item responses
#' @param critVal is the minimum magnitude of the correlation between two items
#' in order for them to be considered psychometric synonyms. Defaults to .60
#' @param anto determines whether psychometric anonyms are returned instead of 
#' psychometric synonyms. Defaults to FALSE
#' 
#' @author Richard Yentes \email{rdyentes@ncsu.edu}
#' @import Hmisc
#' @export
#' @examples 
#' \dontrun{
#' synonyms <- psychsyn(data, .70)
#' 
#' antonyms <- psychsyn(data, anto=T) 
#' }
#' Last updated 22-june-2016
psychsyn <- function(data, critVal=.60, anto=FALSE) {
  data <- as.matrix(data)
  itemPairs <- getItemPairs(data, critVal, anto)
  synonyms <- apply(data,1,synForOne, itemPairs)
  return(synonyms)
}

# Helper function that identifies psychometric synonyms in a given dataset
getItemPairs <- function(data, critVal=.60, anto=FALSE) {
  data <- as.matrix(data)
  critVal <- abs(critVal) #Dummy Proofing

  getCorr <- rcorr(data, type="pearson")
  correlations <- getCorr$r
  correlations[upper.tri(correlations, diag=TRUE)] <- NA

  # Identifying item pairs differs depending on whether the user wants
  # Psychometric Synonyms or Psychometric Antonyms
  if(anto==FALSE) {
    itemPairs <- which(correlations > critVal, arr.in=TRUE)
    if(nrow(itemPairs)==0) {
      stop("No Psychometric Synonyms found.")
    }
  }
  else if(anto==TRUE) {
    itemPairs <- which(correlations < -critVal, arr.in=TRUE)
    if(nrow(itemPairs)==0) {
      stop("No Psychometric Antonyms found.")
    }
  }

  # Make a list of the item pair variable names
  rowNames <- rownames(correlations) # First, collect the Row Names
  colNames <- colnames(correlations) # Then the column names
  matches <- matrix(nrow=nrow(itemPairs), ncol=2) # Instantiate a list to hold them all
  dimnames(matches) <- list(1:nrow(itemPairs), c("xName", "yName")) #tidying up the column names
  for(row in 1:nrow(itemPairs)) { # Loop to fill the list with the names of the variable pairs
    matches[row,"xName"] <- rowNames[itemPairs[row,1]]
    matches[row,"yName"] <- colNames[itemPairs[row,2]]
  }

  return(matches)
}

# Helper function to calculate the within person correlation for a single individual
synForOne <- function(data, itemPairs) {
  # Looping variables
  x_sum = 0
  y_sum = 0

  # Changed to "numeric for cross products"
  cross_products <- vector(mode="numeric", length=nrow(itemPairs))
  x_squares <- vector(mode="numeric", length=nrow(itemPairs))
  y_squares <- vector(mode="numeric", length=nrow(itemPairs))

  for (k in 1:nrow(itemPairs)) {
    cross_products[k] <- data[itemPairs[k,1]]*data[itemPairs[k,2]]
    x_squares[k] <- data[itemPairs[k,1]]^2
    y_squares[k] <- data[itemPairs[k,2]]^2
    x_sum = x_sum + data[itemPairs[k,1]]
    y_sum = y_sum + data[itemPairs[k,2]]
  }

  sum_cp = sum(cross_products)
  sum_squares_x = sum(x_squares)
  sum_squares_y = sum(y_squares)
  x_average = x_sum/nrow(itemPairs)
  y_average = y_sum/nrow(itemPairs)


  numerator = sum_cp - (nrow(itemPairs)*x_average*y_average)
  d1 = sum_squares_x - (nrow(itemPairs)*(x_average^2))
  d2 = sum_squares_y - (nrow(itemPairs)*(y_average^2))
  denominator = sqrt(d1*d2)
  if(denominator == 0){
    denominator = .0000000000001
  }
  result = numerator/denominator

  #if they put same number for all, no variance and NA is the score

  return(result)
}
