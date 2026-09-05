# Facilitate "At Most" Comparison on Logical Vectors

This function facilitates a comparison to check if at most a specified
proportion or count of values in a logical vector evaluate to `TRUE`.

## Usage

``` r
at_most(logical_vec, p = NULL, n = NULL, na.rm = FALSE)
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

`TRUE` if the condition is met for at most the specified proportion or
count, otherwise `FALSE`.

## See also

Other basic_quantifiers:
[`at_least()`](https://iancero.github.io/checkthat/reference/at_least.md),
[`exactly_equal()`](https://iancero.github.io/checkthat/reference/exactly_equal.md),
[`less_than()`](https://iancero.github.io/checkthat/reference/less_than.md),
[`more_than()`](https://iancero.github.io/checkthat/reference/more_than.md)

## Examples

``` r
# Check if at most 20% of values are TRUE
at_most(c(TRUE, FALSE, TRUE, TRUE), p = 0.2) # Returns TRUE
#> [1] FALSE
```
