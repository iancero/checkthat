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


#' Placeholder
#'
#' Here is a thought
#'
#' @param logical_vec Another thought
#' @param na.rm a third thought
#'
#' @return a value
#' @export
#'
prop <- function(logical_vec, na.rm = FALSE) {
  validate_logical_vec(logical_vec)

  if (na.rm) {
    logical_vec <- logical_vec[!is.na(logical_vec)]
  }

  sum(logical_vec) / length(logical_vec)
}

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


validate_logical_vec <- function(logical_vec) {
  if (!is.logical(logical_vec)) {
    stop("logical_vec must be a logical vector")
  }

  if (length(logical_vec) < 1) {
    stop("logical_vec must have length of at least 1")
  }

  if (all(is.na(logical_vec))) {
    warning("All values of logical_vec are NA")
  }

  TRUE
}

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

whenever <- function(is_observed, then_expect, ...) {
  validate_logical_vec(is_observed)

  logical_vec <- then_expect[is_observed]

  if (length(list(...)) == 0) {
    test_result <- all(logical_vec)
    return(test_result)
  }

  some_of(logical_vec, ...)
}


specifically <- function(case, ...) {
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
        "potentially affect multiple rows, use when() instead."
      )
    )
  }

  dots |>
    purrr::map_lgl(~ .x[case_num]) |>
    all()
}


#' At least
#'
#' xxx
#'
#' @param logical_vec a
#' @param p a
#' @param n b
#' @param na.rm c
#'
#' @return d
#' @export
#'
at_least <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`>=`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

more_than <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`>`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

at_most <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`<=`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

less_than <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`<`)(logical_vec, p = p, n = n, na.rm = na.rm)
}

exactly_equal <- function(logical_vec, p = NULL, n = NULL, na.rm = FALSE) {
  quantifier(`==`)(logical_vec, p = p, n = n, na.rm = na.rm)
}
