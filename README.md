
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

``` r
library(checkthat)

mtcars |> 
  arrange(mpg) |> 
  check_that(mpg)
#> Error in check_that(arrange(mtcars, mpg), mpg): could not find function "check_that"
  mutate(rating = if_else(wt < 3, 'light', 'heavy')) |> 
  checkthat:::check_that(
    any(cyl > 4),
    mean(mpg) > 25, 
    all(rating %in% c('light', 'heavy'))
  )
#> Error in mutate(rating = if_else(wt < 3, "light", "heavy")): could not find function "mutate"
```

## Installation

Checkthat is not yet available on CRAN. However, you can install the
development version of checkthat using devtools:

``` r
devtools::install_github('iancero/checkthat')
```

## Example

INSERT MORE COMPLETE EXAMPLES HERE

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
  **unit testing for data**. It is different from checkthat in two ways.
  First, it is a more mature package than checkthat, and is therefore
  likely to be less buggy and more feature rich at this time. Second,
  whereas checkthat is designed to integrate tests into data
  manipulation pipelines, testdat is designed to place tests in separate
  commands or files.
