#' Computes the psychometric antonym score
#'
#' A convenient wrapper that calls \code{psychsyn} with argument \code{anto = TRUE} to compute the psychometric antonym score.
#'
#' @param x is a matrix of item responses
#' @param critval is the minimum magnitude of the correlation between two items
#' in order for them to be considered psychometric synonyms. Defaults to -.60
#' @param n_pairs additionally return the number of item pairs available for each subject. Useful if dataset contains many missing values.
#' @param diag  deprecated. use n_pairs instead.
#'
#' @author Richard Yentes \email{ryentes@gmail.com}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @export
#' @seealso \code{\link{psychsyn}} for the main function, \code{\link{psychsyn_critval}} for a helper that allows to set an
#' adequate critical value for the size of the correlation.
#' @examples
#' antonyms <- psychant(careless_dataset2, .50)
#' antonyms <- psychant(careless_dataset2, .50, n_pairs = TRUE)

psychant <- function(x, critval= -.60, n_pairs=FALSE, diag = NULL) {
  
  if(!missing(diag)) {
    warning("diag argument has been renamed to n_pairs and has been deprecated as of version x.x.x, please use the n_pairs argument instead.")
    n_pairs <- diag
  }
  
  psychsyn(x, critval, anto = TRUE, n_pairs)
}
