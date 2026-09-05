# Check if logical conditions are met some of the time in a logical vector

Designed as a helper function for
[`check_that()`](https://iancero.github.io/checkthat/reference/check_that.md),
this function allows you to check that a certain percentage or count of
TRUE values are observed in a logical vector. It is therefore a more
flexible version of [`all()`](https://rdrr.io/r/base/all.html) or
[`any()`](https://rdrr.io/r/base/any.html).

## Usage

``` r
some_of(logical_vec, ...)
```

## Arguments

- logical_vec:

  A logical vector to be checked.

- ...:

  A set of one or more frequency specifiers (e.g., `at_least = 5`,
  `at_most = .70`).

## Value

A logical value indicating all conditions specified in `...` resolve to
TRUE in the given `logical_vec`.

## Details

This function is designed as a helper function for
[`check_that()`](https://iancero.github.io/checkthat/reference/check_that.md).
It allows you to validate that a certain percentage or count of TRUE
values are observed in a logical vector. It is therefore a more flexible
version of [`all()`](https://rdrr.io/r/base/all.html) or
[`any()`](https://rdrr.io/r/base/any.html).

The named arguments in `...` should correspond to quantifiers (e.g.,
`at_least`, `at_most`) followed by a numeric value representing the
criteria for that quantifier (either an integer count or proportion
between zero and one). For example, `at_least = 2` checks if at least 2
TRUE values are present in `logical_vec`.

Note, specifying exactly 1 in an argument is ambiguous (e.g.,
`at_least = 1`). Because it could represent a count (n = 1) or a
proportion (100%), this value is not allowed in `some_of()` and will
throw an error. If you need to specify exactly 1 (either as a count or a
proportion), please use a more specific quantifier function, such as
`at_least(logical_vec, p = 1)` or `at_least(logical_vec, n = 1)`.

## See also

Other special quantifiers:
[`for_case()`](https://iancero.github.io/checkthat/reference/for_case.md),
[`whenever()`](https://iancero.github.io/checkthat/reference/whenever.md)

## Examples

``` r
logical_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

# Check if at least 2 TRUE values are present
some_of(logical_vec, at_least = 2) # TRUE
#> [1] TRUE

# Check if at most 2 TRUE values are present
some_of(logical_vec, at_most = 2) # FALSE
#> [1] FALSE

# Check if exactly 3 TRUE values are present
some_of(logical_vec, exactly_equal = 3) # TRUE
#> [1] TRUE

# Check if exactly 4 TRUE values are present
some_of(logical_vec, exactly_equal = 3) # FALSE
#> [1] TRUE

# Invalid usage: No specific quantifiers provided (error will be thrown)
try(some_of(logical_vec)) # Error
#> Error in some_of(logical_vec) : 
#>   ... is empty, no specific quantifiers given (e.g., at_least = .50). This is ambiguous. If you intend to test that there are more than 0 true cases, please use any() instead. If you want to test something more specific than that, please specify that in ... (e.g., some_of(letters %in% c('a', 'b'), at_least = 2).
```
