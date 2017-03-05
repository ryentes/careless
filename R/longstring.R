#' Identifies the longest string of identical responses for each observation
#' 
#' Takes a matrix of item responses and, beginning with the second column 
#' compares each column with the previous one to check for matching responses.
#' For each observation, the length of the maximum uninterupted chain of 
#' identical repsonses is returned. 
#' 
#' @param data is a matrix of data
#' @param na is a boolean indicating whether na values are present in the data
#' 
#' @author Richard Yentes \email{rdyentes@ncsu.edu}
#' @export
#' @examples
#' \dontrun{
#' d <- matrix(rep(1:10,10), ncol=10, nrow=10, byrow=T)
#' for(i in 1:10){
#'   d[i,which(1:10 <= i)] <- i
#' }
#' longstring(d)
#' }
#' Last Updated 14-June-2016
longstring <- function(data, na=F) {
  thisString <- rep(1,nrow(data))
  longString <- rep(1,nrow(data))
  if(na==TRUE) { 
    testifsame <- expression(oldVal == newVal | is.na(newVal) | is.na(oldVal))
    testifdiff <- expression(oldVal != newVal | is.na(newVal)) 
  } else {
    testifsame <- expression(oldVal == newVal)
    testifdiff <- expression(oldVal != newVal)
  }
  for(i in 2:ncol(data)){
    oldVal <- data[,i-1]
    newVal <- data[,i]
    same <- eval(testifsame)
    diff <- eval(testifdiff)
    thisString[same] <- thisString[same] + 1
    thisString[diff] <- 1
    t1 <- thisString > longString
    longString[t1] <- thisString[t1]
  }
  return(longString)
}