# Check if a Numeric Value is a Count

This function checks if a numeric value is a count, meaning it is
integer-like and non-negative.

## Usage

``` r
is_count(x, include_zero = TRUE)
```

## Arguments

- x:

  Numeric value to check.

- include_zero:

  Logical, whether to include zero as a valid count.

## Value

`TRUE` if `x` is a count, otherwise `FALSE`.

## See also

[`is_proportion`](https://iancero.github.io/checkthat/reference/is_proportion.md),
[`is_integerlike`](https://iancero.github.io/checkthat/reference/is_integerlike.md),
[`validate_count`](https://iancero.github.io/checkthat/reference/validate_count.md),
[`validate_proportion`](https://iancero.github.io/checkthat/reference/validate_proportion.md)

## Examples

``` r
is_count(0) # TRUE
#> [1] TRUE
is_count(3) # TRUE
#> [1] TRUE
is_count(0, include_zero = FALSE) # FALSE
#> [1] FALSE
is_count(-1) # FALSE
#> [1] FALSE
is_count(1.5) # FALSE
#> [1] FALSE
```
