
<!-- README.md is generated from README.Rmd. Please edit that file -->

# checkthat

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/checkthat)](https://CRAN.R-project.org/package=checkthat)
[![R-CMD-check](https://github.com/iancero/checkthat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iancero/checkthat/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/iancero/checkthat/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iancero/checkthat?branch=main)
<!-- badges: end -->

The goal of checkthat is to integrate testing into your data
manipulation pipelines.

## Installation

You can install the development version of checkthat like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(checkthat)
## basic example code
```

## Alternatives

There are two pre-existing packages that served as inspiration for
checkthat. They are both quite good and, depending on your use case,
might be a better choice for you.

- [**testthat**](https://testthat.r-lib.org/) implements **unit testing
  for packages** and is currently the most popular testing package
  for R. If your goal is to develop a package - rather than conduct a
  data analysis - then testthat will be a much better choice than
  checkthat.

- [**testdat**](https://socialresearchcentre.github.io/testdat/index.html)
  package is inspired by testhat and (like checkthat) also implements
  **unit testing for data manipulation**. It is different from checkthat
  in at least two ways. First, it is currently a more mature package (i.e., less buggy,
  more feature rich) than checkthat. Second, whereas checkthat is
  designed to integrate tests into data manipulation pipelines, testdat
  is designed to place tests in separate commands or files. Thus, as
  checkthat matures, the difference between the two packages will become
  largely stylistic.
