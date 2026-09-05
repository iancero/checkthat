# Facilitate "Less Than" Comparison on Logical Vectors

This function facilitates a comparison to check if less than a specified
proportion or count of values in a logical vector evaluate to `TRUE`.

## Usage

``` r
less_than(logical_vec, p = NULL, n = NULL, na.rm = FALSE)
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

`TRUE` if the condition is met for less than the specified proportion or
count, otherwise `FALSE`.

## See also

Other basic_quantifiers:
[`at_least()`](https://iancero.github.io/checkthat/reference/at_least.md),
[`at_most()`](https://iancero.github.io/checkthat/reference/at_most.md),
[`exactly_equal()`](https://iancero.github.io/checkthat/reference/exactly_equal.md),
[`more_than()`](https://iancero.github.io/checkthat/reference/more_than.md)

## Examples

``` r
# Check if less than 10% of values are TRUE
less_than(c(TRUE, FALSE, FALSE), p = 0.1) # Returns FALSE
#> [1] FALSE
```
