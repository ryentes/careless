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
#' @author Richard Yentes \email{ryentes@gmail.com}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
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

  maha_data <- mahalanobis(x_filtered, center = colMeans(x_filtered), cov = cov(x_filtered))
  d_sq <- rep_len(NA, nrow(x_filtered))
  d_sq[!complete.na] <- maha_data
  
  cut <- stats::qchisq(confidence, ncol(x))
  flagged <- (d_sq > cut)
  
  if (plot) {
    plot_data <- data.frame(row     = seq_len(length(d_sq)),
                            d_sq    = d_sq, 
                            flagged = flagged)

    plot_data <- plot_data[order(plot_data$d_sq, decreasing = TRUE),]

    qqplot(x     = qchisq(ppoints(nrow(x)), df = ncol(x)),
           y     = plot_data$d_sq,
           main  = paste("Q-Q plot of Mahalanobis Distance",
                         "versus Quantiles of Chi-Square"), 
           xlab  = "Quantiles of Chi-Square", 
           ylab  = "Mahalanobis Distance",
           pch   = 16,
           col   = rgb(0, .4, .6, .5))
    abline(0, 1, col = "black", lty = 2)
    text(x      = sort(qchisq(ppoints(nrow(x)), df = ncol(x)), 
                       decreasing = TRUE)[which(plot_data$flagged == TRUE)],
         y      = plot_data$d_sq[which(plot_data$flagged == TRUE)],
         labels = plot_data$row[which(plot_data$flagged == TRUE)],
         pos    = 3)
  }
  if(flag == TRUE) {
    return(data.frame(d_sq = d_sq, flagged = flagged))
  }
  else{ return(d_sq) }
}
