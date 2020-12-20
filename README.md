[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)]
<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/ryentes/careless.svg?branch=master)](https://travis-ci.com/ryentes/careless)
<!-- badges: end -->

<br/>

# careless

Collection of Implementations for Indices of Careless Responding.

## Description

Careless or insufficient effort responding in surveys, i.e. responding to items without regard to their content, is a common occurence in surveys. These types of responses constitute significant problems for data quality leading to distortions in data analysis and hypothesis testing, such as spurious correlations. The R package careless provides solutions designed to detect such careless / insufficient effort responses by allowing easy calculation of indices proposed in the literature. It currently supports the calculation of Longstring, Even-Odd Consistency, Psychometric Synonyms/Antonyms, Mahalanobis Distance, and Intra-individual Response Variability (also termed Inter-item Standard Deviation).

## Current Version

Current version is 1.2.0

### Version Notes
* even-odd consistency score: updated interpretation of even-odd to maintain consistency with other indices. It is now coded such that higher scores indicate higher levels of careless responding. Before, higher scores indicated lower levels of careless responding. This makes the even-odd index consistent with the other indices.
* even-odd consistency score : expanded error and warning messages to provide more information to the user.
* Intra-individual response variability (IRV): now handles NA values.
* psychometric synonym/antonym score: now handles NA values better, and is faster due to implementation of vectorization. The function now uses resampling to adress issues arising from NA values.

## Installing from source

To install this package:

1) Install and load the `devtools` package (if necessary). In R, paste the following into the console, note, you may need to update R or R Studio first:

```r
install.packages('devtools')
library('devtools')
```

3) Download and install the `careless` package from the Github source code.

```r
install_github('ryentes/careless')
library('careless')
```

## APA Citation
Yentes, R.D., & Wilhelm, F. (2018) careless: Procedures for computing indices of careless responding. R packages version 1.2.0 url: https://github.com/ryentes/careless

## License

This package is free and open source software, licensed under MIT License.
