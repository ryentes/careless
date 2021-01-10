#' Find and graph Mahalanobis Distance (D) and flag potential outliers.
#'
#' Takes a matrix of item responses and computes Mahalanobis D. Can additionally return a
#' vector of binary outlier flags.
#' Mahalanobis distance is calculated using the function \code{psych::outlier} of the \pkg{psych}
#' package, an implementation which supports missing values.
#'
#' @param x a matrix of data
#' @param plot Plot the resulting QQ graph
#' @param flag Flag potential outliers using the confidence level specified in parameter \code{confidence}
#' @param confidence The desired confidence level of the result
#' @param na.rm Should missing data be deleted
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#' Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data.
#' \emph{Psychological Methods, 17(3)}, 437-455. \doi{10.1037/a0028085}
#' @export
#' @seealso \code{psych::outlier} on which this function is based.
#' @examples
#' mahad_raw <- mahad(careless_dataset) #only the distances themselves
#' mahad_flags <- mahad(careless_dataset, flag = TRUE) #additionally flag outliers
#' mahad_flags <- mahad(careless_dataset, flag = TRUE, confidence = 0.999) #Apply a strict criterion

mahad <- function(x, plot = TRUE, flag = FALSE, confidence = 0.99, na.rm = TRUE) {
  if(na.rm == FALSE) {
    if(any(is.na(x)) == TRUE) {stop("Some values are NA. Mahalanobis distance was not computed.
                                      Use na.rm = TRUE to use available cases.", call. = FALSE)}
    }
  #remove rows with all NA and issue warning
  complete.na <- apply(x, 1, function(y) { all(is.na(y)) } )
  if(any(complete.na)) {
    warning("Some cases contain only NA values. The Mahalanobis distance will be calculated using available cases.",
            call. = FALSE) }
  x_filtered <- x[!complete.na,]
  
  maha_data <- as.numeric(psych::outlier(x_filtered, plot, bad = 0, na.rm = na.rm))
  maha_data_merge <- rep_len(NA, nrow(x_filtered))
  maha_data_merge[!complete.na] <- maha_data
  
  if(flag == TRUE) {
    cut <- stats::qchisq(confidence, length(maha_data))
    flagged <- (maha_data_merge > cut)
    return(data.frame(maha_data_merge, flagged))
  }
  else{ return(maha_data_merge) }
}
