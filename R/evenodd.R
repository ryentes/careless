#' Calculates the even-odd consistency score
#' 
#' Takes a matrix of item responses and a vector of integers representing the 
#' length each factor. The even-odd consistency score is then computed as the
#' within-person correlation between the even and odd subscales over all the 
#' factors
#' 
#' 
#' @param d is a matrix of data (e.g. survey repsonses)
#' @param factors is a vector of integers specifying the length of each
#' factor in the dataset
#' 
#' @author Richard Yentes \email(rdyentes@ncsu.edu)
#' @export
#' @examples 
#' \dontrun{
#' eo <- evenodd(d, c(rep(10,10)))
#' }
# Last updated 14-June-2016
evenodd <- function(d, factors) {
  #initialize a result dataset
  eo <- vector(length=nrow(d), mode="numeric")
  
  # Loop through each Person
  for(i in 1:nrow(d)) {
    # Initialize an object to hold the factor e/o means for the current person
    f <- matrix(rep(NA, 2*length(factors)), length(factors), ncol=2)
    start <- 1
    
    # loop through each factor
    for(j in 1:length(factors)) {
      if(j>1) start <- start + (factors[j-1])   
      end <- (factors[j]-1) + start
      
      # Subset d with items for the current factor
      s <- d[i,start:end]
      ind <- seq(1:length(colnames(s)))
      eInd <- which(ind %% 2 == 0)
      oInd <- which(ind %% 2 == 1)
      f[j,1] <- mean(t(s[eInd]))
      f[j,2] <- mean(t(s[oInd]))
    }
    
    # Calculate within-person correlation between even and odd sub-scales
    # then apply the Spearman-Brown correction for split-half reliability
    # and store the result in the output vector.
    tmp <- cor(f[,1], f[,2])
    tmp <- (2*tmp)/(1+tmp)
    if(!is.na(tmp) && tmp < -1) tmp <- -1
    eo[i] <- tmp
    rm(f)
  }
  return(eo)
}
