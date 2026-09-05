# Check if a Numeric Value is Integer-like

This function checks if a numeric value is and integer-like scalar,
meaning it is numeric and its length is 1.

## Usage

``` r
is_integerlike(x)
```

## Arguments

- x:

  Numeric value to check.

## Value

`TRUE` if `x` is integer-like, otherwise `FALSE`.

## See also

[`is_proportion`](https://iancero.github.io/checkthat/reference/is_proportion.md),
[`is_count`](https://iancero.github.io/checkthat/reference/is_count.md),
[`validate_proportion`](https://iancero.github.io/checkthat/reference/validate_proportion.md),
[`validate_count`](https://iancero.github.io/checkthat/reference/validate_count.md)

## Examples

``` r
is_integerlike(3) # TRUE
#> [1] TRUE
is_integerlike(3.5) # FALSE
#> [1] FALSE
is_integerlike("3") # FALSE
#> [1] FALSE
is_integerlike(c(1, 2)) # FALSE
#> [1] FALSE
```
