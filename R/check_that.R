#' Check that assertions about a dataframe are true/false
#'
#' This function allows you to test whether a set of assertions about a
#' dataframe are true and to print the results of those tests. It is
#' particularly useful for quality control and data validation.
#'
#' The \code{check_that()} function is designed to work with both base R's
#' existing logical functions, as well as several new functions provided in the
#' checkthat package (see See Also below).
#'
#' In addition, it also provides a data pronoun, \code{.d}. This is a copy of
#' the \code{.data} dataframe provided as the first argument and is useful for
#' testing not only features of specific rows or columns, but of the entire
#' dataframe, see examples.
#'
#' @param .data A dataframe to be tested.
#' @param ... One or more conditions to test on the dataframe. Each condition
#'            should be expressed as a logical expression that evaluates to a
#'            single \code{TRUE} or \code{FALSE} value (e.g., \code{all(x < 3)},
#'            \code{!any(is.na(x))}).
#' @param print Logical. If \code{TRUE}, the results of the tests will be
#'              printed.
#' @param raise_error Logical. If \code{TRUE}, an error will be thrown if any
#'                    test fails. If \code{FALSE}, the evaluation will
#'                    continue even if tests fail. Disabling errors can
#'                    sometimes be useful for debugging, but should generally be
#'                    avoided in finalized checks/tests.
#' @param encourage Logical. If \code{TRUE}, encouraging messages will be
#'                  displayed for tests that pass.
#'
#' @returns (invisibly) the original, unmodified \code{.data} dataframe.
#'
#' @seealso \code{\link{some_of}}, \code{\link{whenever}},
#'          \code{\link{for_case}}
#'
#' @examples
#' example_data <- data.frame(x = 1:5, y = 6:10)
#'
#' # Test a dataframe for specific conditions
#' example_data |>
#'   check_that(
#'     all(x > 0),
#'     !any(y < 5)
#'   )
#'
#' # Use .d pronoun to test aspect of entire dataframe
#' example_data |>
#'   check_that(
#'     nrow(.d) == 5,
#'     "x" %in% names(.d)
#'   )
#'
#' @export
check_that <- function(.data, ..., print = TRUE, raise_error = TRUE,
                       encourage = TRUE) {
  # TODO: add support for grouped data frames

  dots <- rlang::enquos(...)

  data_mask <- rlang::as_data_mask(.data)
  data_mask$.d <- .data

  test_labs <- purrr::map_chr(dots, .f = rlang::as_label)
  test_results <- purrr::map_lgl(dots, .f = ~ rlang::eval_tidy(.x, data_mask))

  if (print) {
    cli_print_checks(test_results, test_labs, encourage = encourage)
  }

  if (raise_error & any(test_results != TRUE)) {
    cli_throw_test_error()
  }

  invisible(.data)
}
