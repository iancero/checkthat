# Whenever one condition is true, check other logical conditions also hold

Designed as a helper function for
[`check_that()`](https://iancero.github.io/checkthat/reference/check_that.md),
this function checks that whenever a certain condition is observed,
other expected conditions hold as well.

## Usage

``` r
whenever(is_observed, then_expect, ...)
```

## Arguments

- is_observed:

  A logical vector indicating the when the observed cases of interest.

- then_expect:

  A logical vector indicating the conditions to be checked for those
  observed cases in `is_observed`.

- ...:

  A set of qualifying logical conditions (e.g., `at_least = .50`) to be
  checked in conjunction with `then_expect`.

## Value

A logical value indicating whether all specified conditions in
`then_expect` hold true, whenever `is_observed` is TRUE.

## Details

This function is designed as a helper function for
[`check_that()`](https://iancero.github.io/checkthat/reference/check_that.md).
It is useful for checking, whenever an event or condition of interest
(`is_observed`) is true, that certain logical conditions (`then_expect`)
also hold true. You can provide additional qualifiers (`...`) to clarify
how often `then_expect` must resolve to TRUE.

## See also

Other special quantifiers:
[`for_case()`](https://iancero.github.io/checkthat/reference/for_case.md),
[`some_of()`](https://iancero.github.io/checkthat/reference/some_of.md)

## Examples

``` r
# whenever() is designed to work with check_that()
df <- data.frame(x = 1:5, y = 6:10)

df |>
  check_that(
    whenever(is_observed = x > 3, then_expect = y > 8),
    whenever(x %in% 2:3, y > 6, at_least = .50) # qualifying condition
  )
#> ✔ all data checks passing

# whenever() can also work outside check_that()
x <- 1:5
y <- 6:10

whenever(x > 3, y > 9, at_least = 1 / 2) # TRUE
#> [1] TRUE
```
