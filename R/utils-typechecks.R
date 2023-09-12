#' Check if a Numeric Value is Integer-like
#'
#' This function checks if a numeric value is and integer-like scalar, meaning
#' it is numeric and its length is 1.
#'
#' @param x Numeric value to check.
#' @returns \code{TRUE} if \code{x} is integer-like, otherwise \code{FALSE}.
#'
#' @seealso \code{\link{is_proportion}}, \code{\link{is_count}},
#'          \code{\link{validate_proportion}}, \code{\link{validate_count}}
#'
#' @examples
#' is_integerlike(3) # TRUE
#' is_integerlike(3.5) # FALSE
#' is_integerlike("3") # FALSE
#' is_integerlike(c(1, 2)) # FALSE
#' @export
is_integerlike <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  if (length(x) != 1) {
    return(FALSE)
  }

  x == round(x)
}

#' Check if a Numeric Value is a Proportion
#'
#' This function checks if a numeric value is a proportion scalar, meaning it
#' is numeric and within the range of 0 to 1 (inclusive).
#'
#' @param x Numeric value to check.
#' @returns \code{TRUE} if \code{x} is a proportion, otherwise \code{FALSE}.
#'
#' @seealso \code{\link{is_integerlike}}, \code{\link{is_count}},
#'          \code{\link{validate_proportion}}, \code{\link{validate_count}}
#'
#' @examples
#' is_proportion(0.5) # TRUE
#' is_proportion(1.2) # FALSE
#' is_proportion(-0.2) # FALSE
#' @export
is_proportion <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  if (length(x) != 1) {
    return(FALSE)
  }

  (x >= 0) & (x <= 1)
}

#' Validate a Proportion Value
#'
#' This function validates whether a numeric value is a valid proportion scalar
#' (ranging from 0 to 1, inclusive).
#'
#' @param x Numeric value to validate as a proportion.
#' @returns \code{TRUE} if \code{x} is a valid proportion, otherwise it throws
#'          an error.
#'
#' @seealso \code{\link{is_proportion}}, \code{\link{is_count}},
#'          \code{\link{validate_count}}, \code{\link{is_integerlike}}
#'
#' @examples
#' validate_proportion(0.5) # TRUE
#' try(validate_proportion(1.2)) # Error
#' @export
validate_proportion <- function(x) {
  if (!is_proportion(x)) {
    stop("Not a valid proportion")
  }

  TRUE
}

#' Check if a Numeric Value is a Count
#'
#' This function checks if a numeric value is a count, meaning it is
#' integer-like and non-negative.
#'
#' @param x Numeric value to check.
#' @param include_zero Logical, whether to include zero as a valid count.
#' @returns \code{TRUE} if \code{x} is a count, otherwise \code{FALSE}.
#'
#' @seealso \code{\link{is_proportion}}, \code{\link{is_integerlike}},
#'          \code{\link{validate_count}}, \code{\link{validate_proportion}}
#'
#' @examples
#' is_count(0) # TRUE
#' is_count(3) # TRUE
#' is_count(0, include_zero = FALSE) # FALSE
#' is_count(-1) # FALSE
#' is_count(1.5) # FALSE
#' @export
is_count <- function(x, include_zero = TRUE) {
  if (!is_integerlike(x)) {
    return(FALSE)
  }

  if (!include_zero) {
    return(x > 0)
  }

  x >= 0
}

#' Validate a Count Value
#'
#' This function validates whether a numeric value is a valid count (integer of
#' zero or greater).
#'
#' @param x Numeric value to validate as a count.
#' @param include_zero Logical, whether to include zero as a valid count.
#' @returns \code{TRUE} if \code{x} is a valid count, otherwise it throws an
#'          error.
#'
#' @seealso \code{\link{is_count}}, \code{\link{is_proportion}},
#'          \code{\link{validate_proportion}}, \code{\link{is_integerlike}}
#'
#' @examples
#' validate_count(0) # TRUE
#' validate_count(3) # TRUE
#' try(validate_count(0, include_zero = FALSE)) # Error: Not a valid count
#' try(validate_count(-1)) # Error: Not a valid count value.
#' @export
validate_count <- function(x, include_zero = TRUE) {
  if (!is_count(x, include_zero = include_zero)) {
    stop("Not a valid count value (i.e., integer of zero or greater).")
  }

  TRUE
}


#' Validate a Logical Vector
#'
#' Validates a logical vector to ensure it meets specific criteria:
#' - Must have a length of at least 1.
#' - Must be a logical-type vector.
#' - If all values are NA, it will raise a warning.
#'
#' @param logical_vec Logical vector to validate.
#' @returns \code{TRUE} if the logical vector is valid, otherwise it throws an
#'          error.
#'
#' @seealso \code{\link{is_proportion}}, \code{\link{is_count}},
#'          \code{\link{validate_proportion}}, \code{\link{validate_count}}
#'
#' @examples
#' validate_logical_vec(c(TRUE, FALSE, TRUE)) # TRUE
#' try(validate_logical_vec(c())) # Error
#' validate_logical_vec(c(NA, NA)) # Warning
#' @export
validate_logical_vec <- function(logical_vec) {
  if (length(logical_vec) < 1) {
    stop("logical_vec must have length of at least 1")
  }

  if (!is.logical(logical_vec)) {
    stop("logical_vec must be a logical vector")
  }

  if (all(is.na(logical_vec))) {
    warning("All values of logical_vec are NA")
  }

  TRUE
}
