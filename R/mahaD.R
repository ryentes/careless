#' Find and graph Mahalanobis Distance (D) and flag potential outliers.
#'
#' Takes a matrix of item responses and computes Mahalanobis D. Can additionally return a
#' vector of binary outlier flags.
#' Mahalanobis distance is calculated using the function \code{psych::outlier} of the \pkg{psych} package,
#' an implementation which supports missing values.
#'
#' @param x a matrix of data
#' @param plot Plot the resulting QQ graph
#' @param flag Flag potential outliers using the confidence level specified in parameter \code{confidence}
#' @param confidence The desired confidence level of the result
#' @param na.rm Should missing data be deleted
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#'Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data.
#' Psychological Methods, 17(3), 437-455. https://doi.org/10.1037/a0028085
#' @export
#' @seealso \code{psych::outlier} on which this function is based.
#' @examples
#' mahadRaw <- mahaD(carelessDataset) #only the distances themselves
#' mahadFlags <- mahaD(carelessDataset, flag = T) #additionally flag outliers
#' mahadFlags <- mahaD(carelessDataset, flag = T, confidence = 0.999) #flag with more strict criterion

mahaD <- function(x, plot = TRUE, flag = FALSE, confidence = 0.99, na.rm = TRUE) {
  raw <- as.numeric(psych::outlier(x, plot, bad = 0, na.rm = na.rm))
  if(flag == TRUE) {
    cut <- qchisq(confidence, ncol(x))
    flagged <- (raw > cut)
    return(data.frame(raw, flagged))
  }
  else{ return(raw) }
}
