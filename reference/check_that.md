# Check that assertions about a dataframe are true/false

This function allows you to test whether a set of assertions about a
dataframe are true and to print the results of those tests. It is
particularly useful for quality control and data validation.

## Usage

``` r
check_that(.data, ..., print = TRUE, raise_error = TRUE, encourage = TRUE)
```

## Arguments

- .data:

  A dataframe to be tested.

- ...:

  One or more conditions to test on the dataframe. Each condition should
  be expressed as a logical expression that evaluates to a single `TRUE`
  or `FALSE` value (e.g., `all(x < 3)`, `!any(is.na(x))`).

- print:

  Logical. If `TRUE`, the results of the tests will be printed.

- raise_error:

  Logical. If `TRUE`, an error will be thrown if any test fails. If
  `FALSE`, the evaluation will continue even if tests fail. Disabling
  errors can sometimes be useful for debugging, but should generally be
  avoided in finalized checks/tests.

- encourage:

  Logical. If `TRUE`, encouraging messages will be displayed for tests
  that pass.

## Value

(invisibly) the original, unmodified `.data` dataframe.

## Details

The `check_that()` function is designed to work with both base R's
existing logical functions, as well as several new functions provided in
the checkthat package (see See Also below).

In addition, it also provides a data pronoun, `.d`. This is a copy of
the `.data` dataframe provided as the first argument and is useful for
testing not only features of specific rows or columns, but of the entire
dataframe, see examples.

## See also

[`some_of`](https://iancero.github.io/checkthat/reference/some_of.md),
[`whenever`](https://iancero.github.io/checkthat/reference/whenever.md),
[`for_case`](https://iancero.github.io/checkthat/reference/for_case.md)

## Examples

``` r
example_data <- data.frame(x = 1:5, y = 6:10)

# Test a dataframe for specific conditions
example_data |>
  check_that(
    all(x > 0),
    !any(y < 5)
  )
#> ✔ all data checks passing

# Use .d pronoun to test aspect of entire dataframe
example_data |>
  check_that(
    nrow(.d) == 5,
    "x" %in% names(.d)
  )
#> ✔ all data checks passing
```
