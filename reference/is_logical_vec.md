# Check if a Vector is a Valid Logical Vector

This function checks if a given vector is a valid logical vector. A
valid logical vector is one that contains only logical values (`TRUE` or
`FALSE`), has a length of at least 1, and does not consist entirely of
missing values (`NA`).

## Usage

``` r
is_logical_vec(logical_vec)
```

## Arguments

- logical_vec:

  A vector to be evaluated.

## Value

`TRUE` if `logical_vec` is a valid logical vector, otherwise `FALSE`.

## Examples

``` r
# Check if a valid logical vector
is_logical_vec(c(TRUE, FALSE, TRUE)) # Returns TRUE
#> [1] TRUE

# Check if an empty vector
is_logical_vec(c()) # Returns FALSE
#> [1] FALSE

# Check if a vector with missing values
is_logical_vec(c(TRUE, FALSE, NA)) # Returns TRUE
#> [1] TRUE
is_logical_vec(c(NA, NA, NA)) # Returns FALSE
#> [1] FALSE
```
