#' careless: A package providing procedures for computing indices of careless responding
#'
#' Careless or insufficient effort responding in surveys, i.e. responding to items without regard to their content, 
#' is a common occurence in surveys. These types of responses constitute significant problems for data quality 
#' leading to distortions in data analysis and hypothesis testing, such as spurious correlations. 
#' The R package careless provides solutions designed to detect such careless / insufficient effort responses
#' by allowing easy calculation of indices proposed in the literature. 
#' It currently supports the calculation of Longstring, Even-Odd Consistency, Psychometric Synonyms/Antonyms, 
#' Mahalanobis Distance, and Intra-individual Response Variability (also termed Inter-item Standard Deviation).
#' 
#' 
#' @section Statistical outlier function:
#' 
#' \itemize{
#' \item \code{\link{mahad}} computes Mahalanobis Distance, 
#'  which gives the distance of a data point relative to the center of a multivariate distribution.
#'  }
#'  
#' @section Consistency indices:
#' \itemize{
#' \item \code{\link{evenodd}} computes the Even-Odd Consistency Index. It divides unidimensional scales using an even-odd split; 
#' two scores, one for the even and one for the odd subscale, are then computed as the average response across subscale items.
#' Finally, a within-person correlation is computed based on the two sets of subscale scores for each scale.
#' \item \code{\link{psychsyn}} computes the Psychometric Synonyms Index, or, alternatively, the Psychometric Antonyms Index. 
#' Psychometrical synonyms are item pairs which are correlated highly positively, 
#' whereas psychometric antonyms are item pairs which are correlated highly negatively.
#' A within-person correlation is then computed based on these item pairs.
#' \item \code{\link{psychant}} is a convenience wrapper for \code{\link{psychsyn}} that computes psychological antonyms.
#' \item \code{\link{psychsyn_critval}} is a helper designed to set an adequate critical value (i.e. magnitude of correlation) 
#' for the psychometric synonyms/antonyms index.
#' }
#' 
#' @section Response pattern functions:
#' \itemize{
#'  \item \code{\link{longstring}} computes the longest (and optionally, average) length of consecutive identical responses given.
#'  \item \code{\link{irv}} computes the Intra-individual Response Variability (IRV), 
#'  the "standard deviation of responses across a set of consecutive item responses for an individual" (Dunn et al. 2018)
#'  }
#'  
#' @section Datasets:
#' \itemize{
#'   \item \code{\link{careless_dataset}}, a simulated dataset with 200 observations and 10 subscales of 5 items each.
#'   \item \code{\link{careless_dataset2}}, a simulated dataset with 1000 observations and 10 subscales of 10 items each.
#'   }
#'   The sample datasets differ in the types of careless responding simulated.
#' 
#' @author Richard Yentes \email{rdyentes@ncsu.edu}, Francisco Wilhelm \email{franciscowilhelm@gmail.com}
#' 
#' 
#' @docType package
#' @name careless
NULL