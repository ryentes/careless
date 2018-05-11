datasetSampler <- function() {

  #SCALE SAMPLER: calculates a set of item responses for a scale
  scalesampler <- function(responsetype) {
    if(responsetype < 9) {
      x <- sample(2:4, 1)
      out <- sample((x-1):(x+1), 5, replace = T, prob = c(0.2, 0.6, 0.2)) #valid response - centered around a trait score of 2,3 or 4
    }
    else if(responsetype == 9) {
      out <- sample(5, 5, replace = T) #random response - all answers equally likely
    }
    else if(responsetype == 10) {
      out <- sample(5,5, replace = T, prob = c(0.02, 0.02, 0.02, 0.02, 0.9)) #"straightlining" response type
    }
    return(out)
  }

  subjectsampler <- function(times) {
    responsetype <- sample(10,1) #determine responsetype of subject
    subject <- replicate(times, scalesampler(responsetype)) #create answers to  /times scales
    subject <- as.vector(t(subject)) #turn matrix to one-dimensional vector
    return(subject)
  }

  outputDataset <- t(replicate(200, subjectsampler(10)))
  return(outputDataset)
}
