
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/checkthat)](https://CRAN.R-project.org/package=checkthat) -->

# checkthat

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/iancero/checkthat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iancero/checkthat/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/iancero/checkthat/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iancero/checkthat?branch=main)
<!-- badges: end -->

Most people regularly check that their data are valid after a
manipulation. This process is critical to valid analysis results later
on. Are there any impossible values in a newly created column? Does a
dataframe have the correct number of columns and rows after a join?

Despite its importance, the data checking process is usually conducted
informally by hand or by eye - instead of in code (e.g., as in a unit
test). If something in your pipeline is modified later, how can you be
sure everything that comes after that change remains valid?

The **checkthat** philosophy is that you already perform good data
checks and you should keep doing it. But those checks would be even
better if they lived in the code, rather than in your head. Checkthat
therefore provides functions that closely resemble the checks you
already do by hand or by eye, so that it is easy for you to also express
them in code as you go.

## Installation

The release version is available on CRAN.

``` r
install.packages("checkthat")
```

The development version is available on Github.

``` r
devtools::install("iancero/checkthat")
```

## Basic usage

Checkthat’s main function is `check_that(.data, ...)`, which takes a
dataframe as its first argument, followed by any number of assertions
you want to check for that dataframe.

When all checks pass, you get a brief message confirming that’s the
case.

``` r
library(checkthat)

mtcars |>
  check_that(
    all(cyl > 2),
    !any(is.na(mpg))
  )
#> ✔ all data checks passing
```

When at least one check fails, `check_that()` throws an error, halting
the potentially risky execution of subsequent code. It then gives you
get a detailed breakdown of what the outcome was for each test.

``` r
mtcars |>
  check_that(
    all(cyl > 2),
    any(mpg > 35)
  )
#> 
#> ── Data Checks ─────────────────────────────────────────────────────────────────
#> 
#> ✔ all(cyl > 2) --> TRUE
#> ✖ any(mpg > 35) --> FALSE
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> Error in `cli_throw_test_error()`:
#> ! At least one data check failed.
```

The `check_that()` function is designed to work with both base R’s
existing logical functions (e.g., `all()`, `any()`), as well it’s own
set of more special helper functions. Theses helper functions are
designed to be both readable and to mirror in code what you already do
manually by eye-balling a dataset.

``` r
mtcars |>
  check_that(
    some_of(cyl > 4, at_least = .30, at_most = 25),
    whenever(wt < 3, then_expect = mpg > 19),
    for_case(2, mpg == 21, hp == 110)
  )
#> ✔ all data checks passing
```

## Tidyverse pipelines

The `check_that()` function always invisibly returns the same `.data`
you gave it (always unmodified). This allows you to easily integrate it
directly into your data manipulation pipelines.

``` r
library(dplyr)

new_mtcars <- mtcars |>
  select(mpg) |>
  mutate(km_per_litre = 0.425 * mpg) |>
  check_that(max(km_per_litre) < 15)
#> ✔ all data checks passing

head(new_mtcars)
#>                    mpg km_per_litre
#> Mazda RX4         21.0       8.9250
#> Mazda RX4 Wag     21.0       8.9250
#> Datsun 710        22.8       9.6900
#> Hornet 4 Drive    21.4       9.0950
#> Hornet Sportabout 18.7       7.9475
#> Valiant           18.1       7.6925
```

## Checking a pipeline at multiple points

Because it returns the same dataframe it received, `check_that()` can
also be used at multiple points in a single pipeline. That way, you can
check that multi-step processes are unfolding according to plan. This is
be especially important for data tasks that are sensitive to the order
of operations, or for checks on intermediate data that wont be available
at the end.

Consider a surprisingly tricky example. Imagine we wanted to (1) create
a factor variable (`type`) designating cars as either small (`"sm"`) or
large (`"lg"`) based on their weight (`wt`). Further imagine that we
then (2) planned to filter in only the small cars and (3) calculate
their mean `mpg` as our `desired_mpg`. This value might then be used to
inform a personal purchase decision or perhaps to establish an industry
benchmark for a manufacturer.

The resulting data pipeline should be simple, but let’s use
`check_that()` at multiple points to be safe.

1.  We wont have access to the `wt` variable at the end of the pipeline.
    So, right after we use `wt` to compute `type`, we immediately check
    that all the weights in the `"sm"` group are less than those in the
    `"lg"` group, as intended.
2.  At the end, we check that our `desired_mpg` is within a plausible
    range.

Here, the first check throws an error and stopS the pipeline. It also
saves us from an inaccurate `desired_mpg` that the second check would
not have caught.

``` r
mtcars |>
  mutate(type = factor(wt < 3, labels = c("sm", "lg"), ordered = TRUE)) |>
  check_that(max(wt[type == "sm"]) <= min(wt[type == "lg"])) |>
  filter(type == "sm") |>
  summarise(desired_mpg = mean(mpg)) |>
  check_that(desired_mpg > 15)
#> 
#> ── Data Checks ─────────────────────────────────────────────────────────────────
#> 
#> ✖ max(wt[type == "sm"]) <= min(wt[type == "lg"]) --> FALSE
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> Error in `cli_throw_test_error()`:
#> ! At least one data check failed.
```

What happened? A quick reading of
`factor(wt < 3, labels = c("sm", "lg"), ordered = TRUE)` seems like it
would correctly assign cars to the correct group. However, the labels
are out of order in the function call.[^1] As a result, the heavy cars
are mistakenly labelled `"sm"` and vice-versa.[^2]

Importantly, this mistake (a) would have given us an erroneously low
`desired_mpg` and (b) would have gone undetected by our final
`check_that(desired_mpg > 15)`. It was a call to `check_that()` earlier
in the pipeline that caught the error and prevented us from drawing an
bad conclusion about our data later on.

## Helper functions

Checkthat’s philosophy is your existing data checks by eye are probably
already good. Their only major problem is that they live in your head
and not in your code. So, checkthat provides a range of helper functions
to work alongside base R’s existing collection (e.g., `all()`, `any()`).
These include both some basic and more special varieties.

### Basic helpers

The most basic helpers are just syntactic sugar around R’s existing
comparison operators: `=`, `<`, `<=`, `>`, `>=`. Each of them takes a
logical vector as its first argument and requires you to specify a
proportion (`p`) or count (`n`) of those values that must be true.

``` r
mtcars |>
  check_that(
    at_least(mpg < 35, p = .95),
    more_than(hp == 110, n = 2),
    exactly_equal(cyl == 6, n = 7),
    less_than(wt > 3, p = .75),
    at_most(is.na(mpg), n = 3),
  )
#> ✔ all data checks passing
```

### Special helpers

The remaining helpers include `some_of()`, `whenever()`, and
`for_case()` and are more flexible than their basic counterparts.
They’re optimized for the kind of semi-approximate data checking you are
likely already doing by eye.

For most people, this involves a general sense of what most of the data
should look like most of the time, but not exact knowledge of specific
proportions or counts. For example, you might have good reason to think
`some_of()` the `cyl` values should be greater than 4, but you don’t
know exactly how many. However, you do know it should probably be
`at_least` 30%, but `at_most` 25 total cases in your dataset. Anything
outside that range would be implausible and so you want to guard it with
`check_that()`.

``` r
mtcars |>
  check_that(
    some_of(cyl > 4, at_least = .30, at_most = 25),
    whenever(is_observed = wt < 3, then_expect = mpg > 19),
    for_case(2, mpg == 21, hp == 110)
  )
#> ✔ all data checks passing
```

Just like unit tests for production code, the tests created with these
special helper functions will be technically imperfect and leave some
(possibly important) scenarios addressed. After all, there’s a big range
of possibilities between `at_least = .30` and `at_most = 25`, and some
of them might involve an undetected data problem.

However, checkthat takes the position that imperfect tests are still
valuable informative and you should be able to take advantage of them.
For example, if you have reasons to be concerned about the data in your
column crossing the `at_most = 25`, you should be able to quickly and
easily write that test with a combination of `check_that()` and
`some_of()`.

Moreover, a world of *no tests at all* is much worse than a world of
*some tests that fail to cover every case*. With that in mind,
checkthat’s special helper functions are designed to bring you from *not
writing down any tests in your code* to *quickly and easily coding the
tests you already do by eye*.

## Checking the whole dataframe

In addition to concerns about the individual rows or columns in your
data, you may also want to perform checks on the entire dataframe in
question. For those cases, `check_that()` provides the `.d` pronoun,
which works similarly to `.x` in the **purrr** package.

In short, `.d` is a copy of the data you provided to `check_that()`,
which you can use to write checks about the whole dataset.

``` r
mtcars |>
  check_that(
    nrow(.d) > 10,
    "mpg" %in% names(.d)
  )
#> ✔ all data checks passing
```

This is especially useful for operations that could change the shape of
your dataset (e.g., pivots, nests, joins). In the case of pivoting, you
might want to check that the dataset have the correct anticipated
dimensions.

``` r
library(tidyr)

mtcars |>
  check_that(ncol(.d) == 11, nrow(.d) == 32) |> # original dimensions
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "values"
  ) |>
  check_that(ncol(.d) == 2, nrow(.d) == 32 * 11) # check that cols became rows
#> ✔ all data checks passing
#> ✔ all data checks passing
```

After a join, you may want to check that there is a new column in the
expected location, but also that there are no unanticipated new rows.

``` r
cyl_ratings_df <- data.frame(cyl = c(4, 6, 8), group = c("A", "B", "C"))

mtcars |>
  left_join(cyl_ratings_df, by = "cyl") |>
  check_that(
    ncol(.d) == 12, # check that there's one new column
    names(.d)[length(names(.d))] == "group", # check new column is "group"
    nrow(.d) == 32 # check that no new rows
  )
#> ✔ all data checks passing
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
  is inspired by testhat and - like checkthat - also implements **unit
  testing for data**. It is different from checkthat in two ways. First,
  it is a more mature package than checkthat, and is therefore
  potentially more stable in the near term. Second, whereas checkthat is
  designed to directly embed your tests into data manipulation
  pipelines, testdat is designed to place tests in separate code blocks
  or even files.

- [**validate**](https://github.com/data-cleaning/validate) is another
  well-developed and popular data checking package. It can work is ways
  that are similar to checkthat (and it also has its own analogous
  `check_that()` function). However, the validate package is primarily
  dedicated to specifying your data validation rules in advance, rather
  than the on-the-fly approach for checkthat. This is ideal when you
  want your validation rules to be repeatable. For example, you might
  want `Sepal.Width < 0.5*Sepal.Length` to always be `TRUE`, no matter
  where you are in the analysis. In contrast, f you want simple checks
  that you write concurrently with the data manipulation code itself,
  then you might prefer checkthat.

[^1]: The `factor()` function maps `wt < 3 == FALSE --> 1` and
    `TRUE --> 2` in this case because `FALSE < TRUE`. So, when
    `wt < 3 == TRUE`, `factor(..., labels = c("sm", "lg"))` will assign
    the 2nd label (mistakenly, from our perspective).

[^2]: Full disclosure, I made this mistake when preparing this example,
    and it was the `check_that()` function that pointed it out to me.
    So, I decided to include it.
