# Facilitate "More Than" Comparison on Logical Vectors

This function facilitates a comparison to check if more than a specified
proportion or count of values in a logical vector evaluate to `TRUE`.

## Usage

``` r
more_than(logical_vec, p = NULL, n = NULL, na.rm = FALSE)
```

## Arguments

- logical_vec:

  A logical vector.

- p:

  Proportion value (0 to 1) to compare against.

- n:

  Count value (integer) to compare against.

- na.rm:

  Logical. Should missing values be removed before calculation?

## Value

`TRUE` if the condition is met for more than the specified proportion or
count, otherwise `FALSE`.

## See also

Other basic_quantifiers:
[`at_least()`](https://iancero.github.io/checkthat/reference/at_least.md),
[`at_most()`](https://iancero.github.io/checkthat/reference/at_most.md),
[`exactly_equal()`](https://iancero.github.io/checkthat/reference/exactly_equal.md),
[`less_than()`](https://iancero.github.io/checkthat/reference/less_than.md)

## Examples

``` r
# Check if more than 70% of values are TRUE
more_than(c(TRUE, TRUE, FALSE, TRUE), p = 0.7) # Returns TRUE
#> [1] TRUE
```
