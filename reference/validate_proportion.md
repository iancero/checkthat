# Validate a Proportion Value

This function validates whether a numeric value is a valid proportion
scalar (ranging from 0 to 1, inclusive).

## Usage

``` r
validate_proportion(x)
```

## Arguments

- x:

  Numeric value to validate as a proportion.

## Value

`TRUE` if `x` is a valid proportion, otherwise it throws an error.

## See also

[`is_proportion`](https://iancero.github.io/checkthat/reference/is_proportion.md),
[`is_count`](https://iancero.github.io/checkthat/reference/is_count.md),
[`validate_count`](https://iancero.github.io/checkthat/reference/validate_count.md),
[`is_integerlike`](https://iancero.github.io/checkthat/reference/is_integerlike.md)

## Examples

``` r
validate_proportion(0.5) # TRUE
#> [1] TRUE
try(validate_proportion(1.2)) # Error
#> Error in validate_proportion(1.2) : Not a valid proportion
```
