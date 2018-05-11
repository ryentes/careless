#' Calculates the even-odd consistency score
#'
#' Takes a matrix of item responses and a vector of integers representing the
#' length each factor. The even-odd consistency score is then computed as the
#' within-person correlation between the even and odd subscales over all the
#' factors.
#'
#' @param x a matrix of data (e.g. survey responses)
#' @param factors a vector of integers specifying the length of each
#' factor in the dataset
#' @param diag optionally returns a column with the number of even/odd pairs
#' for which no comparison could be computed because of NAs. Useful for datasets with many missing values.
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#'Johnson, J. A. (2005). Ascertaining the validity of individual protocols
#'from web-based personality inventories. Journal of Research in Personality, 39, 103-129. doi:10.1016/j.jrp.2004.09.009
#' @export
#' @examples
#' carelessEo <- evenodd(carelessDataset, rep(5,10))
#' carelessEoDiag <- evenodd(carelessDataset, rep(5,10), diag = T)

evenodd <- function(x, factors, diag = FALSE) {
  #initialize a result dataset
  eo <- vector(length=nrow(x), mode="numeric")
  eoMissing <- vector(length=nrow(x), mode="numeric")

  # Loop through each Person
  for(i in 1:nrow(x)) {
    # Initialize an object to hold the factor e/o means for the current person
    f <- matrix(rep(NA, 2*length(factors)), length(factors), ncol=2)
    start <- 1

    # loop through each factor
    for(j in 1:length(factors)) {
      if(j>1) start <- start + (factors[j-1])
      end <- (factors[j]-1) + start

      # Subset x with items for the current factor
      s <- x[i,start:end]
      ind <- seq(1:length(colnames(s)))
      eInd <- which(ind %% 2 == 0)
      oInd <- which(ind %% 2 == 1)
      f[j,1] <- mean(t(s[eInd]), na.rm = T)
      f[j,2] <- mean(t(s[oInd]), na.rm = T)
    }

    # Calculate within-person correlation between even and odd sub-scales
    # then apply the Spearman-Brown correction for split-half reliability
    # and store the result in the output vector.
    eoMissing[i] <- sum(is.na(apply(f, 1, sum))) #number of even/odd pairs for which no comparison can be computed because of NAs
    tmp <- cor(f[,1], f[,2], use ="pairwise.complete.obs")
    tmp <- (2*tmp)/(1+tmp)
    if(!is.na(tmp) && tmp < -1) tmp <- -1
    eo[i] <- tmp
    rm(f)
  }
  if(diag == F) {return(eo)}
  else {return(data.frame(eo, eoMissing))}
}
