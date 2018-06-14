## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE-----------------------------------------------------
library(careless)
knitr::opts_chunk$set(fig.width=6, fig.height=6) 

## ------------------------------------------------------------------------
x <- c(4,4,4,3,3,3,3,3,4,4)
print(x)

## ------------------------------------------------------------------------
careless_long <- longstring(careless_dataset2)
boxplot(careless_long, main = "Boxplot of Longstring index")

## ------------------------------------------------------------------------
print(x)

## ------------------------------------------------------------------------
careless_long <- longstring(careless_dataset2, avg = T)
boxplot(careless_long$avg, main = "Boxplot of Averagestring index")

## ------------------------------------------------------------------------
x <- c(4,5,4,5,4,5,4,5,4,5)
print(x)

## ------------------------------------------------------------------------
sd(x)

## ------------------------------------------------------------------------
careless_irv <- irv(careless_dataset2, split = TRUE, num.split = 4)
head(careless_irv)

## ------------------------------------------------------------------------
psychsyn_cor <- psychsyn_critval(careless_dataset2)
head(psychsyn_cor)

## ------------------------------------------------------------------------
sum(psychsyn_cor$Freq > .60, na.rm = TRUE)

## ------------------------------------------------------------------------
example_psychsyn <- psychsyn(careless_dataset2, critval = .60)
hist(example_psychsyn, main = "Histogram of psychometrical synonyms index")

## ------------------------------------------------------------------------
psychant_cor <- psychsyn_critval(careless_dataset2, anto = TRUE)
head(psychant_cor)

## ------------------------------------------------------------------------
careless_eo <- evenodd(careless_dataset2, factors = rep(10,10))
hist(careless_eo, main = "Histogram of even-odd consistency index")

## ------------------------------------------------------------------------
careless_mahad <- mahad_raw <- mahad(careless_dataset)

