# Validate a Count Value

This function validates whether a numeric value is a valid count
(integer of zero or greater).

## Usage

``` r
validate_count(x, include_zero = TRUE)
```

## Arguments

- x:

  Numeric value to validate as a count.

- include_zero:

  Logical, whether to include zero as a valid count.

## Value

`TRUE` if `x` is a valid count, otherwise it throws an error.

## See also

[`is_count`](https://iancero.github.io/checkthat/reference/is_count.md),
[`is_proportion`](https://iancero.github.io/checkthat/reference/is_proportion.md),
[`validate_proportion`](https://iancero.github.io/checkthat/reference/validate_proportion.md),
[`is_integerlike`](https://iancero.github.io/checkthat/reference/is_integerlike.md)

## Examples

``` r
validate_count(0) # TRUE
#> [1] TRUE
validate_count(3) # TRUE
#> [1] TRUE
try(validate_count(0, include_zero = FALSE)) # Error: Not a valid count
#> Error in validate_count(0, include_zero = FALSE) : 
#>   Not a valid count value (i.e., integer of zero or greater).
try(validate_count(-1)) # Error: Not a valid count value.
#> Error in validate_count(-1) : 
#>   Not a valid count value (i.e., integer of zero or greater).
```
