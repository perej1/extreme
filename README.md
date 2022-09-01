
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extreme

<!-- badges: start -->

[![R-CMD-check](https://github.com/perej1/extreme/workflows/R-CMD-check/badge.svg)](https://github.com/perej1/extreme/actions)
<!-- badges: end -->

## Overview

*extreme* is a R package for estimating multivariate elliptical
![(1-p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%281-p%29 "(1-p)")-quantile
regions. Currently, there are two available estimators. The first one is
based on sample quantile and works well when the quantile region is on
range of the data. The second one is based on a well known extreme
quantile estimator for heavy-tailed distributions and works well for
small values of
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p").
For example, it can be that
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")
is so small that the corresponding quantile is on the right-hand side of
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

## Example

For an example, see repository
[perej1/elliptical-sim](https://github.com/perej1/elliptical-sim).
