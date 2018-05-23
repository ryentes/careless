#' Calculates the intra-individual response variability (IRV)
#'
#' A recently proposed index, somewhat similar to the LongString index, the IRV is the
#' "standard deviation of responses across a set of consecutive item responses for an individual"
#' (Dunn, Heggestad, Shanock, & Theilgard, 2018, p. 108).
#' By default, the IRV is calculated across all columns of the input data. Additionally it can be applied to different subsets of the data.
#' This can detect degraded response quality which occurs only in a certain section of the questionnaire (usually the end).
#'
#' @param x a matrix of data (e.g. survey responses)
#' @param split boolean indicating whether to additionally calculate the IRV on subsets of columns (of equal length).
#' @param numSplit the number of subsets the data is to be split in.
#' @author Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#' Dunn, A. M., Heggestad, E. D., Shanock, L. R., & Theilgard, N. (2018).
#' Intra-individual Response Variability as an Indicator of Insufficient Effort Responding:
#' Comparison to Other Indicators and Relationships with Individual Differences.
#' Journal of Business and Psychology, 33(1), 105-121. https://doi.org/10.1007/s10869-016-9479-0
#' @export
#' @examples
#' # calculate the irv over all items
#' irvTotal <- irv(carelessDataset)
#'
#' #calculate the irv over all items + calculate the irv for each quarter of the questionnaire
#' irvSplit <- irv(carelessDataset, split = TRUE, numSplit = 4)
#' boxplot(irvSplit$irv4) #produce a boxplot of the IRV for the fourth quarter

irv <- function(x, split = FALSE, numSplit = 3) {
  out <- apply(x, 1, stats::sd)
  if(split == TRUE) {
    chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
    splitX <- apply(x, 1, chunk, numSplit)
    outSplit <- t(replicate(nrow(x), rep(NA, numSplit)))
    colnames(outSplit) <- paste0("irv",1:numSplit)
    for(k in 1:nrow(outSplit)) {
      splitXSingle <- splitX[[k]]
      outSplit[k,] <- unlist(lapply(splitXSingle, stats::sd), use.names = FALSE)
    }
      outSplit <- data.frame(out, outSplit)
      colnames(outSplit)[1] <- "irvTotal"
      return(outSplit)} else {
      return(out)
    }
}
