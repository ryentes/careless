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
#' @param diag optionally returns a column with the number of available (i.e., non-missing) even/odd pairs per observation.
#' Useful for datasets with many missing values.
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#' Johnson, J. A. (2005). Ascertaining the validity of individual protocols
#' from web-based personality inventories. \emph{Journal of Research in Personality, 39}, 103-129. \doi{10.1016/j.jrp.2004.09.009}
#' @export
#' @examples
#' careless_eo <- evenodd(careless_dataset, rep(5,10))
#' careless_eodiag <- evenodd(careless_dataset, rep(5,10), diag = TRUE)

evenodd <- function(x, factors, diag = FALSE) {
  #initialize a result dataset
  warning("Computation of even-odd has changed for consistency of interpretation
          with other indices. This change occurred in version 1.2.0. A higher 
          score now indicates a greater likelihood of careless responding. If 
          you have previously written code to cut score based on the output of 
          this function, you should revise that code accordingly.")

  if(length(factors) == 1) {
    stop("You have called even-odd with only a single factor. \n The even-odd method requires multiple factors to work correctly.",
            call. = FALSE) }
  if(sum(factors) > ncol(x)) {
    stop("The number of items specified by 'factors' exceeds the number of columns in 'x'.",
            call. = FALSE) }
  if(sum(factors) != ncol(x)) {
    warning("The number of items specified by 'factors' does not match the number of columns in 'x'. \n Please check if this is what you want.",
            call. = FALSE) }

  # initalize empty list for persons holding the persons even scores and odd scores
  eo_vals <- vector("list", nrow(x))

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
      e_ind <- which(ind %% 2 == 0)
      o_ind <- which(ind %% 2 == 1)
      f[j,1] <- mean(t(s[e_ind]), na.rm = TRUE)
      f[j,2] <- mean(t(s[o_ind]), na.rm = TRUE)
    }
    # assign the even and odd values to eo_vals
    eo_vals[[i]] <- f
  }


  #calculate number of even/odd pairs for which no comparison can be computed because of NAs
  eo_missing <- lapply(eo_vals, function(i) sum(!is.na(apply(i, 1, sum))))

  # scan for persons for which no even-odd can be calculated when all values are same, leading to
  # a correlation of NA because there is no variance/standard deviation.
  eo_sdzero <-  lapply(eo_vals, function(i) apply(i, 2, stats::sd))
  eo_sdzero <- sapply(eo_sdzero, function(i) any(i == 0, na.rm = TRUE))
  if(any(eo_sdzero, na.rm = TRUE)) warning("One or more observations have zero variance in even and/or odd values. \nThis results in NA values for these observations.\nIncluding more factors may alleviate this issue.",
                             call. = FALSE)

  # Calculate within-person correlation between even and odd sub-scales
  # then apply the Spearman-Brown correction for split-half reliability
  # and store the result in the output vector.
  eo_cor <- sapply(eo_vals, function(f) {
    # suppressWarnings for standard deviation 0 which happens when each value-pairs is same
    val <- suppressWarnings(stats::cor(f[, 1], f[, 2], use = "pairwise.complete.obs"))
    val <- (2 * val) / (1 + val) #split-half
    if (!is.na(val) && val < -1) val <- -1
    return(val)
  })

  # transform eo  such that higher scores are indicating carelessness
  eo_cor = 0 - eo_cor

  if(diag == FALSE) {return(eo_cor)}
  else {return(data.frame(eo_cor, eo_missing))}
}
