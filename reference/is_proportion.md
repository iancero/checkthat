# Check if a Numeric Value is a Proportion

This function checks if a numeric value is a proportion scalar, meaning
it is numeric and within the range of 0 to 1 (inclusive).

## Usage

``` r
is_proportion(x)
```

## Arguments

- x:

  Numeric value to check.

## Value

`TRUE` if `x` is a proportion, otherwise `FALSE`.

## See also

[`is_integerlike`](https://iancero.github.io/checkthat/reference/is_integerlike.md),
[`is_count`](https://iancero.github.io/checkthat/reference/is_count.md),
[`validate_proportion`](https://iancero.github.io/checkthat/reference/validate_proportion.md),
[`validate_count`](https://iancero.github.io/checkthat/reference/validate_count.md)

## Examples

``` r
is_proportion(0.5) # TRUE
#> [1] TRUE
is_proportion(1.2) # FALSE
#> [1] FALSE
is_proportion(-0.2) # FALSE
#> [1] FALSE
```
