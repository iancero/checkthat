# Facilitate "At Least" Comparison on Logical Vectors

This function facilitates a comparison to check if at least a specified
proportion or count of values in a logical vector evaluate to `TRUE`.

## Usage

``` r
at_least(logical_vec, p = NULL, n = NULL, na.rm = FALSE)
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

`TRUE` if the condition is met for at least the specified proportion or
count, otherwise `FALSE`.

## See also

Other basic_quantifiers:
[`at_most()`](https://iancero.github.io/checkthat/reference/at_most.md),
[`exactly_equal()`](https://iancero.github.io/checkthat/reference/exactly_equal.md),
[`less_than()`](https://iancero.github.io/checkthat/reference/less_than.md),
[`more_than()`](https://iancero.github.io/checkthat/reference/more_than.md)

## Examples

``` r
# Check if at least 50% of values are TRUE
at_least(c(TRUE, TRUE, FALSE), p = 0.5) # Returns TRUE
#> [1] TRUE
```
