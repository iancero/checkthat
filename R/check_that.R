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
check_that <- function(.data, ...) {
  UseMethod("check_that")
}

#' @export
check_that.default <- function(.data, ..., print = TRUE, raise_error = TRUE, as_df = FALSE) {

  dots <- rlang::enquos(..., .named = TRUE)
  mask <- new_check_mask(.data)

  results <- dots |> 
    purrr::map_lgl(rlang::eval_tidy, data = mask) |> 
    tibble::enframe(name = 'test', value = 'result')
 
  report_checks(.data, results, print = print, raise_error = raise_error, as_df = as_df)
}

#' @export
check_that.data.frame <- function(.data, ..., print = TRUE, raise_error = TRUE, as_df = FALSE){
  
  dots <- rlang::enquos(..., .named = TRUE)
  
  results <- .data |> 
    dplyr::summarize(!!!dots) |> 
    tidyr::pivot_longer(
      cols = all_of(names(dots)),
      names_to = 'test',
      values_to = 'result'
    )
  
  report_checks(.data, results, print = print, raise_error = raise_error, as_df = as_df)
}

new_check_mask <- function(.data){
  # TODO: document this and figure out the best placement
    if(is.list(.data) | is.data.frame(.data)){
        mask <- rlang::as_data_mask(.data)
    } else {
        mask <- rlang::as_data_mask(NULL)
    }
    mask$.d <- .data

    mask
}

report_checks <- function(.data, results, print, raise_error, as_df) {
  # TODO: add documentation
  if(as_df) {
    return(results)
  }

  if(print) {
    print(results)
  }
  
  if (raise_error) {
    stopifnot('At least one test failed' = all(results$result == TRUE))
  }
  
  invisible(.data)
}