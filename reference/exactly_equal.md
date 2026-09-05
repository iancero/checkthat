# Facilitate "Exactly Equal" Comparison on Logical Vectors

This function facilitates a comparison to check if the proportion or
count of values in a logical vector is exactly equal to a specified
value.

## Usage

``` r
exactly_equal(logical_vec, p = NULL, n = NULL, na.rm = FALSE)
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

`TRUE` if the proportion or count of values is exactly equal to the
specified value, otherwise `FALSE`.

## See also

Other basic_quantifiers:
[`at_least()`](https://iancero.github.io/checkthat/reference/at_least.md),
[`at_most()`](https://iancero.github.io/checkthat/reference/at_most.md),
[`less_than()`](https://iancero.github.io/checkthat/reference/less_than.md),
[`more_than()`](https://iancero.github.io/checkthat/reference/more_than.md)

## Examples

``` r
# Check if all values are TRUE
exactly_equal(c(TRUE, TRUE, TRUE), p = 1.0) # Returns TRUE
#> [1] TRUE
```
