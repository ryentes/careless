#' Computes the psychometric antonym score
#'
#' A convenient wrapper that calls \code{psychsyn} with argument \code{anto = TRUE} to compute the psychometric antonym score.
#'
#' @param x is a matrix of item responses
#' @param critval is the minimum magnitude of the correlation between two items
#' in order for them to be considered psychometric synonyms. Defaults to -.60
#' @param diag additionally return the number of item pairs available for each subject. Useful if dataset contains many missing values.
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @export
#' @seealso \code{\link{psychsyn}} for the main function, \code{\link{psychsyn_critval}} for a helper that allows to set an
#' adequate critical value for the size of the correlation.
#' @examples
#' antonyms <- psychant(careless_dataset2, .50)
#' antonyms <- psychant(careless_dataset2, .50, diag = TRUE)

psychant <- function(x, critval= -.60, diag=FALSE) {
  psychsyn(x, critval, anto = TRUE, diag)
}
