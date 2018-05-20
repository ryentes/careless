#' Evaluates each row of a matrix for outlier status.
#'
#' Takes a matrix of item repsonses and computes Mahalanobis distance. Returns a
#' vector of binary outlier flags by default. If the raw options is specified
#' the function returns a vector of raw Mahalanobis distance scores.
#'
#'
#' @param data is a matrix of data
#' @param confidence is the desired confidence level of the result
#' (default =.95)
#' @param raw determines whether the result vector contains outlier flags or raw
#' Mahalanobis distance values (default = FALSE)
#'
#' @author Richard Yentes \email{rdyentes@ncsu.edu}
#' @export
#' @examples
#' \dontrun{
#' x <- outlier(data)
#' mahd <- outlier(data,raw=TRUE)
#' z <- outlier(data, confidence=.999)
#' }
# Last updated 8-june-2015
outlier <- function(data, confidence=.95, raw=FALSE) {
    mu <- colMeans(data)
    Sx <- cov(data)

    d2 <- mahalanobis(data, mu, Sx)

    if(raw) {
        return(d2)
    } else {
        cut <- qchisq(confidence, ncol(data))
        return(d2 > cut)
    }
}
