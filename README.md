
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extreme

<!-- badges: start -->

[![R-CMD-check](https://github.com/perej1/extreme/workflows/R-CMD-check/badge.svg)](https://github.com/perej1/extreme/actions)
<!-- badges: end -->

## Overview

CURRENTLY THE DEVELOPMENT OF THE PACKAGE IS ON HOLD. I am not sure if I
will continue developing the package. At least it has served as a lesson
for me about basics of creating an R package.

*extreme* is a R package for estimating multivariate elliptical
*(1-p)*-quantile regions. Currently, there are two available estimators.
The first one is based on sample quantile and works well when the
quantile region is on range of the data. The second one is based on a
well known extreme quantile estimator for heavy-tailed distributions and
works well for small values of *p*. For example, it can be that *p* is
so small that the corresponding quantile is on the right-hand side of
all the observations.

## Installation

You can install the development version of extreme from Github with:

``` r
# install.packages("devtools")
devtools::install_github("perej1/extreme")
```

## Usage

Essentially, the package provides two functionalities.

1.  Generate a sample from an elliptical distribution with function
    `relliptical`.

2.  Calculate quantile region estimate with function `qreg`. Function
    `qreg` returns an `ellipsoidq` object representing the elliptical
    quantile estimate.
