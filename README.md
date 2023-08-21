
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
devtools::install_github('iancero/checkthat')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
stop('Banana')
```

``` r
library(checkthat)

df <- mtcars
df$id <- rownames(mtcars)
rownames(df) <- NULL

df <- df[, c('id', names(df)[names(df) != 'id'])]

mtcars |> 
  check_that(
    all(cyl >= 4),
    any(cyl < 8),
    
    some(cyl < 8, at_least = 5),
    some(disp > 200, at_least = .10, less_than = 75),
    
    whenever(
      is_observed = cyl == 6, 
      then_expect = mpg < 50.0),
    
    specifically(case = 2, mpg == 21, cyl < 8),
    specifically(case = id == 'Mazda RX4 Wag', hp == 110)
  )

df2
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
  package is inspired by testhat and - like checkthat - also implements
  **unit testing for data manipulation**. It is different from checkthat
  in two ways. First, it is a more mature package (i.e., less buggy,
  more feature rich) than checkthat. Second, whereas checkthat is
  designed to integrate tests into data manipulation pipelines, testdat
  is designed to place tests in separate commands or files. Thus, as
  checkthat matures, the difference between the two packages will become
  largely stylistic.
