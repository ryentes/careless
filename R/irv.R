#' Calculates the intra-individual response variability (IRV)
#'
#' The IRV is the "standard deviation of responses across a set of consecutive item responses for an individual"
#' (Dunn, Heggestad, Shanock, & Theilgard, 2018, p. 108).
#' By default, the IRV is calculated across all columns of the input data. Additionally it can be applied to different subsets of the data.
#' This can detect degraded response quality which occurs only in a certain section of the questionnaire (usually the end).
#' Whereas Dunn et al. (2018) propose to mark persons with \emph{low} IRV scores as outliers - reflecting straightlining responses,
#' Marjanovic et al. (2015) propose to mark persons with \emph{high} IRV scores - reflecting highly random responses (see References).
#'
#' @param x a matrix of data (e.g. survey responses)
#' @param split boolean indicating whether to additionally calculate the IRV on subsets of columns (of equal length).
#' @param num.split the number of subsets the data is to be split in.
#' @author Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' @references
#' Dunn, A. M., Heggestad, E. D., Shanock, L. R., & Theilgard, N. (2018).
#' Intra-individual Response Variability as an Indicator of Insufficient Effort Responding:
#' Comparison to Other Indicators and Relationships with Individual Differences.
#' \emph{Journal of Business and Psychology, 33(1)}, 105-121. \doi{10.1007/s10869-016-9479-0}
#' 
#' Marjanovic, Z., Holden, R., Struthers, W., Cribbie, R., & Greenglass, E. (2015). 
#' The inter-item standard deviation (ISD): An index that discriminates between conscientious and random responders. 
#' \emph{Personality and Individual Differences}, 84, 79-83. \doi{10.1016/j.paid.2014.08.021}
#' @export
#' @examples
#' # calculate the irv over all items
#' irv_total <- irv(careless_dataset)
#'
#' #calculate the irv over all items + calculate the irv for each quarter of the questionnaire
<<<<<<< HEAD
#' irv_split <- irv(careless_dataset, split = TRUE, num.split = 4)
#' boxplot(irv_split$irv4) #produce a boxplot of the IRV for the fourth quarter

irv <- function(x, split = FALSE, num.split = 3) {
  out <- apply(x, 1, stats::sd)
  if(split == TRUE) {
    chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
    split_x <- apply(x, 1, chunk, num.split)
    out_split <- t(replicate(nrow(x), rep(NA, num.split)))
    colnames(out_split) <- paste0("irv",1:num.split)
    for(k in 1:nrow(out_split)) {
      split_x_single <- split_x[[k]]
      out_split[k,] <- unlist(lapply(split_x_single, stats::sd), use.names = FALSE)
=======
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
>>>>>>> master
    }
      out_split <- data.frame(out, out_split)
      colnames(out_split)[1] <- "irvTotal"
      return(out_split)} else {
      return(out)
    }
}
