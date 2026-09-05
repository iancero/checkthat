# Calculate Proportion of TRUE Values in a Logical Vector

This function calculates the proportion of `TRUE` values in a logical
vector.

## Usage

``` r
prop(logical_vec, na.rm = FALSE)
```

## Arguments

- logical_vec:

  A logical vector.

- na.rm:

  Logical. Should missing values be removed before calculation? Behaves
  similar to [`base::mean`](https://rdrr.io/r/base/mean.html), removing
  missing values from both the numerator and denominator of the
  proportion calculation.

## Value

The proportion of `TRUE` values in the logical vector.

## Examples

``` r
prop(c(TRUE, TRUE, FALSE, TRUE)) # Returns 0.75
#> [1] 0.75
prop(c(TRUE, FALSE, TRUE, FALSE, NA), na.rm = TRUE) # Returns 0.5
#> [1] 0.5
```
