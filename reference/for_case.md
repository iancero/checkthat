# Check if Logical Conditions Hold for a Specific Data Row

Designed as a helper function for
[`check_that()`](https://iancero.github.io/checkthat/reference/check_that.md),
this function checks whether user-supplied logical conditions hold true
for a specific data row.

## Usage

``` r
for_case(case, ...)
```

## Arguments

- case:

  A row number or a logical vector identifying the specific data row(s)
  to check. If a logical vector, it must have exactly 1 TRUE element
  (i.e., that can be used to infer the row of interest).

- ...:

  A set of logical conditions to be checked.

## Value

A logical value indicating whether ALL specified conditions hold true
for the specified data row (i.e., `case`).

## Details

This function is useful for checking if certain logical conditions are
met for a specific data row in your dataset. You can provide one or more
logical conditions as arguments, and the function will evaluate them for
the specified row.

If you provide a row number (`case`), the function will check the
conditions for that specific row. If `case` is a logical vector, it will
check the conditions for rows where `case` is TRUE. Note, when `case` is
a logical vector, it must have exactly one TRUE element that can then be
used to infer the row of interest. Internally, this is done with a call
to [`which()`](https://rdrr.io/r/base/which.html).

If the specified `case` is not a valid count (i.e., a row number) or
does not satisfy the condition `length(which(case)) == 1`, the function
will throw an error.

## See also

Other special quantifiers:
[`some_of()`](https://iancero.github.io/checkthat/reference/some_of.md),
[`whenever()`](https://iancero.github.io/checkthat/reference/whenever.md)

## Examples

``` r
# for_case is designed primarily as a helper function for check_that
sample_data <- data.frame(id = c(11, 22, 33), group = c("A", "B", "C"))

sample_data |>
  check_that(
    for_case(2, group == "B"), # case given as number
    for_case(id == 22, group == "B") # case given as logical vector
  )
#> ✔ all data checks passing

# for_case will technically work with simple vectors too
backwards_letters <- rev(letters)
for_case(3, backwards_letters == "x") # TRUE
#> [1] TRUE
```
