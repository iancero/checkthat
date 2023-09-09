#' Aggregate Logical Vector Values
#'
#' This internal function aggregates logical vector values based on the
#' specified type of aggregation requested (proportion of summation).
#' It is used by the package internally and not meant to be directly accessed by
#' users.
#'
#' @param logical_vec A logical vector to be aggregated.
#' @param type A character string indicating the type of aggregation.
#'             Possible values: "p" for proportions, "n" for counts.
#' @param na.rm Logical. Should missing values be removed when aggregating?
#'
#' @return Depending on the \code{type} parameter, returns either the proportion
#'         of \code{TRUE} values in the logical vector or the count of
#'         \code{TRUE} values.
#'
#' @examples
#' logical_vector <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
#' aggregator(logical_vector, type = "p")
#' aggregator(logical_vector, type = "n")
#'
#' @keywords internal
#' @noRd
aggregator <- function(logical_vec, type = NULL, na.rm = FALSE) {
  validate_logical_vec(logical_vec)

  if (length(type) != 1) {
    stop("length(type) != 1")
  }

  if (!type %in% c("p", "n")) {
    stop("type must be either 'p' (for proportions) or 'n' (for counts)")
  }

  switch(type,
    p = prop(logical_vec, na.rm = na.rm),
    n = sum(logical_vec, na.rm = na.rm)
  )
}

#' Generate Quantification Functions
#'
#' This internal function is used to generate quantification functions for the
#' package. It takes an operator function as an argument and returns a
#' quantification function that operates on logical vectors using the provided
#' operator.
#'
#' @param operator A binary operator function (e.g., \code{<}, \code{>},
#'                \code{==}).
#'
#' @return A quantification function that takes logical vector(s) and a criteria
#'        value (either proportion or count) and performs the specified
#'        operation.
#'
#' @examples
#' # Generate a function for checking if at least 50% of values are TRUE
#' at_least_50_percent <- quantifier(`>=`)(p = 0.5)
#'
#' # Use the generated function on a logical vector
#' at_least_50_percent(c(TRUE, TRUE, FALSE, TRUE)) # Returns TRUE
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # The quantifier function generates other convenience functions like so
#' at_least <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
#'   quantifier(`>=`)(logical_vec, p = p, n = n, na.rm = na.rm)
#' }
#'
#' # Generate a function for checking if at least 50% of values are TRUE
#' at_least_50_percent <- quantifier(`>=`)(p = 0.5)
#'
#' # Use the generated function on a logical vector
#' at_least_50_percent(c(TRUE, TRUE, FALSE, TRUE)) # Returns TRUE
quantifier <- function(operator) {
  function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
    validate_logical_vec(logical_vec)

    if (is.null(p) & is.null(n)) {
      stop("Either p or n must be non-NULL")
    }

    if (!(is.null(p) | is.null(n))) {
      stop("At least one of p or n must be non-NULL")
    }

    if (!is.null(p)) {
      validate_proportion(p)
      type <- "p"
      criteron_val <- p
    }

    if (!is.null(n)) {
      validate_count(n)
      type <- "n"
      criteron_val <- n
    }

    operator(
      e1 = aggregator(logical_vec, type, na.rm = na.rm),
      e2 = criteron_val
    )
  }
}


#' Calculate Proportion of TRUE Values in a Logical Vector
#'
#' This function calculates the proportion of \code{TRUE} values in a logical
#' vector.
#'
#' @param logical_vec A logical vector.
#' @param na.rm Logical. Should missing values be removed before calculation?
#'              Behaves similar to \code{base::mean}, removing missing values
#'              from both the numerator and denominator of the proportion
#'              calculation.
#'
#' @return The proportion of \code{TRUE} values in the logical vector.
#'
#' @examples
#' prop(c(TRUE, TRUE, FALSE, TRUE)) # Returns 0.75
#' prop(c(TRUE, FALSE, TRUE, FALSE, NA), na.rm = TRUE) # Returns 0.5
#'
#' @keywords exported
#' @export
prop <- function(logical_vec, na.rm = FALSE) {


  # @importFrom checkthat validate_logical_vec
  validate_logical_vec(logical_vec)

  if (na.rm) {
    logical_vec <- logical_vec[!is.na(logical_vec)]
  }

  sum(logical_vec) / length(logical_vec)
}


#' Determine if a Value Represents a Proportion (p) or Count (n)
#'
#' This internal function determines if a given value represents a proportion
#' (p) or a count (n). It is used by the package internally to handle cases
#' where a value can be interpreted as either a proportion or a count. This
#' function helps ensure consistent and safe handling of such values and is used
#' as a check in several other package functions.
#'
#' @param num A numeric value to be evaluated.
#' @param error_on_1 Logical. Should an error be raised if \code{num} is equal
#'                    to 1? Defaults to \code{TRUE}. This is important in cases
#'                    where 1 is an ambiguous case (e.g., it could represent
#'                    a probability of exactly 1.0 or a count of exactly 1).
#' @param error_on_nothing Logical. Should an error be raised if \code{num} is
#'                          not a valid proportion (p) or count (n)? Defaults to
#'                          \code{TRUE}.
#'
#' @return A character string: "p" if \code{num} represents a proportion, "n" if
#'          it represents a count, and \code{FALSE} if neither applies.
#'
#' @examples
#'
#' # Determine if 0.5 is a proportion (p) or a count (n)
#' is_p_or_n(0.5)  # Returns "p"
#'
#' # Determine if 10 is a proportion (p) or a count (n)
#' is_p_or_n(10)  # Returns "n"
#'
#' # Determine if 1 is a proportion (p) or a count (n) and allow error on 1
#' is_p_or_n(1, error_on_1 = TRUE)  # Throws an error
#'
#' # Determine if 1 is a proportion (p) or a count (n) and allow error on nothing
#' is_p_or_n(1, error_on_nothing = TRUE)  # Throws an error
#'
#' @keywords internal
#' @noRd
is_p_or_n <- function(num, error_on_1 = TRUE, error_on_nothing = TRUE) {
  if (num == 1 & error_on_1) {
    stop(
      paste(
        "Cannot safely determine if 1 should be treated as n = 1 or p = 1.",
        "In cases like this, you usually want to use a more specific function.",
        "For example, if you intend p = 1.0, all() is likely better",
        "or you can also use the _p() family of functions",
        "(e.g., exactly_p(), at_least_p(), at_most_p()).",
        "Alternatively, if you meant, n = 1, use the _n() family of functions",
        "(e.g., exactly_n(), at_least_n(), at_most_n())."
      )
    )
  }

  if (is_proportion(num)) {
    return("p")
  }

  if (is_count(num)) {
    return("n")
  }

  if (error_on_nothing) {
    stop("Value is neither a valid proportion (p) or count (n)")
  }

  FALSE
}

#' Check if a Vector is a Valid Logical Vector
#'
#' This function checks if a given vector is a valid logical vector. A valid logical vector
#' is one that contains only logical values (\code{TRUE} or \code{FALSE}), has a length of
#' at least 1, and does not consist entirely of missing values (\code{NA}).
#'
#' @param logical_vec A vector to be evaluated.
#'
#' @return \code{TRUE} if \code{logical_vec} is a valid logical vector, otherwise \code{FALSE}.
#'
#' @examples
#' # Check if a valid logical vector
#' is_logical_vec(c(TRUE, FALSE, TRUE))  # Returns TRUE
#'
#' # Check if an empty vector
#' is_logical_vec(c())  # Returns FALSE
#'
#' # Check if a vector with missing values
#' is_logical_vec(c(TRUE, FALSE, NA))  # Returns TRUE
#' is_logical_vec(c(NA, NA, NA))  # Returns FALSE
#'
#' @export
is_logical_vec <- function(logical_vec) {
  if (!is.logical(logical_vec)) {
    return(FALSE)
  }

  if (length(logical_vec) < 1) {
    return(FALSE)
  }

  if (all(is.na(logical_vec))) {
    return(FALSE)
  }

  TRUE
}


#' Title
#'
#' @param logical_vec
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
some_of <- function(logical_vec, ...) {
  validate_logical_vec(logical_vec)

  dots <- rlang::list2(...)

  if (length(dots) < 1) {
    stop(
      paste(
        "... is empty, no specific quantifiers given (e.g., at_least = .50).",
        "This is ambiguous. If you intend to test that there are more",
        "than 0 true cases, please use any() instead.",
        "If you want to test something more specific than that, please specify",
        "that in ... (e.g., some_of(letters %in% c('a', 'b'), at_least = 2)."
      )
    )
  }

  if (any(rlang::names2(dots) == "")) {
    stop("all arguments must be named")
  }

  query_params <- purrr::map(dots, ~ list(val = .x, type = is_p_or_n(.x)))
  function_template <- "{.y}(logical_vec, {.x[['type']]} = {.x[['val']]})"

  query_params |>
    purrr::imap_chr(~ glue::glue(function_template)) |>
    purrr::map_lgl(~ eval(parse(text = .x))) |>
    all()
}


#' Title
#'
#' @param is_observed
#' @param then_expect
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
whenever <- function(is_observed, then_expect, ...) {
  validate_logical_vec(is_observed)
  validate_logical_vec(then_expect)

  logical_vec <- then_expect[is_observed]

  if (length(list(...)) == 0) {
    test_result <- all(logical_vec)
    return(test_result)
  }

  some_of(logical_vec, ...)
}


#' Title
#'
#' @param case
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
for_case <- function(case, ...) {
  dots <- rlang::list2(...)

  if (is_count(case, include_zero = F)) {
    case_num <- case
  } else if (length(which(case)) == 1) {
    case_num <- which(case)
  } else {
    stop(
      paste(
        "case is either not a valid count (i.e., a row number)",
        "or 'length(which(case)) == 1' != TRUE",
        "(e.g., letters == 'g'). If you want to check specific situations that",
        "potentially affect multiple rows, use whenever() instead."
      )
    )
  }

  dots |>
    purrr::map_lgl(~ .x[case_num]) |>
    all()
}


#' Facilitate "At Least" Comparison on Logical Vectors
#'
#' This function facilitates a comparison to check if at least a specified proportion or count of values in a logical vector
#' evaluate to \code{TRUE}.
#'
#' @param logical_vec A logical vector.
#' @param p Proportion value (0 to 1) to compare against.
#' @param n Count value (integer) to compare against.
#' @param na.rm Logical. Should missing values be removed before calculation?
#'
#' @return \code{TRUE} if the condition is met for at least the specified proportion or count, otherwise \code{FALSE}.
#' @examples
#' # Check if at least 50% of values are TRUE
#' at_least(c(TRUE, TRUE, FALSE), p = 0.5) # Returns TRUE
#'
#' @family quantifiers
#'
#' @export
at_least <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`>=`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

#' Facilitate "More Than" Comparison on Logical Vectors
#'
#' This function facilitates a comparison to check if more than a specified proportion or count of values in a logical vector
#' evaluate to \code{TRUE}.
#'
#' @param logical_vec A logical vector.
#' @param p Proportion value (0 to 1) to compare against.
#' @param n Count value (integer) to compare against.
#' @param na.rm Logical. Should missing values be removed before calculation?
#'
#' @return \code{TRUE} if the condition is met for more than the specified proportion or count, otherwise \code{FALSE}.
#' @examples
#' # Check if more than 70% of values are TRUE
#' more_than(c(TRUE, TRUE, FALSE, TRUE), p = 0.7)  # Returns TRUE
#'
#' @family quantifiers
#'
#' @export
more_than <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`>`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

#' Facilitate "At Most" Comparison on Logical Vectors
#'
#' This function facilitates a comparison to check if at most a specified proportion or count of values in a logical vector
#' evaluate to \code{TRUE}.
#'
#' @param logical_vec A logical vector.
#' @param p Proportion value (0 to 1) to compare against.
#' @param n Count value (integer) to compare against.
#' @param na.rm Logical. Should missing values be removed before calculation?
#'
#' @return \code{TRUE} if the condition is met for at most the specified proportion or count, otherwise \code{FALSE}.
#' @examples
#' # Check if at most 20% of values are TRUE
#' at_most(c(TRUE, FALSE, TRUE, TRUE), p = 0.2)  # Returns TRUE
#'
#' @family quantifiers
#'
#' @export
at_most <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`<=`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

#' Facilitate "Less Than" Comparison on Logical Vectors
#'
#' This function facilitates a comparison to check if less than a specified proportion or count of values in a logical vector
#' evaluate to \code{TRUE}.
#'
#' @param logical_vec A logical vector.
#' @param p Proportion value (0 to 1) to compare against.
#' @param n Count value (integer) to compare against.
#' @param na.rm Logical. Should missing values be removed before calculation?
#'
#' @return \code{TRUE} if the condition is met for less than the specified proportion or count, otherwise \code{FALSE}.
#' @examples
#' # Check if less than 10% of values are TRUE
#' less_than(c(TRUE, FALSE, FALSE), p = 0.1)  # Returns FALSE
#'
#' @family quantifiers
#'
#' @export
less_than <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`<`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

#' Facilitate "Exactly Equal" Comparison on Logical Vectors
#'
#' This function facilitates a comparison to check if the proportion or count of values in a logical vector is exactly equal
#' to a specified value.
#'
#' @param logical_vec A logical vector.
#' @param p Proportion value (0 to 1) to compare against.
#' @param n Count value (integer) to compare against.
#' @param na.rm Logical. Should missing values be removed before calculation?
#'
#' @return \code{TRUE} if the proportion or count of values is exactly equal to the specified value, otherwise \code{FALSE}.
#' @examples
#' # Check if all values are TRUE
#' exactly_equal(c(TRUE, TRUE, TRUE), p = 1.0)  # Returns TRUE
#'
#' @family quantifiers
#'
#' @export
exactly_equal <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`==`)(logical_vec, p = p, n = n, na.rm = na.rm)
}
