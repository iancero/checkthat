# Validate a Logical Vector

Validates a logical vector to ensure it meets specific criteria:

- Must have a length of at least 1.

- Must be a logical-type vector.

- If all values are NA, it will raise a warning.

## Usage

``` r
validate_logical_vec(logical_vec)
```

## Arguments

- logical_vec:

  Logical vector to validate.

## Value

`TRUE` if the logical vector is valid, otherwise it throws an error.

## See also

[`is_proportion`](https://iancero.github.io/checkthat/reference/is_proportion.md),
[`is_count`](https://iancero.github.io/checkthat/reference/is_count.md),
[`validate_proportion`](https://iancero.github.io/checkthat/reference/validate_proportion.md),
[`validate_count`](https://iancero.github.io/checkthat/reference/validate_count.md)

## Examples

``` r
validate_logical_vec(c(TRUE, FALSE, TRUE)) # TRUE
#> [1] TRUE
try(validate_logical_vec(c())) # Error
#> Error in validate_logical_vec(c()) : 
#>   logical_vec must have length of at least 1
validate_logical_vec(c(NA, NA)) # Warning
#> Warning: All values of logical_vec are NA
#> [1] TRUE
```
