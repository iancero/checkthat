#' Internal Function to Customize the CLI Theme for checkthat Messages
#'
#' This is an internal function that is not intended for external use. It
#' customizes the Command Line Interface (CLI) theme for messages displayed
#' by the checkthat package.
#'
#' @noRd
#'
#' @returns A customized CLI theme for checkthat messages.
checkthat_cli_theme <- function() {
  theme <- cli::builtin_theme()
  theme[["h1"]][["color"]] <- "grey40"
  theme[[".alert-success"]][["color"]] <- "grey40"
  theme[[".alert-danger"]][["color"]] <- "grey40"
  theme[["body"]][["color"]] <- "grey40"

  theme
}

#' Internal Function to Retrieve a Random Encouraging Message
#'
#' This is an internal function that is not intended for external use.
#' It retrieves a random encouraging message from a predefined list of messages.
#'
#' @noRd
#'
#' @returns A randomly selected string containing an encouraging message.
encouraging_message <- function() {
  encouraging_messages <- c(
    "You've got this!",
    "Believe in yourself",
    "Keep pushing forward",
    "Every step is progress",
    "Stay determined, success is near",
    "Challenges help you grow",
    "You're making it happen!",
    "Keep your focus, you'll get there",
    "Trust the process",
    "Hard work pays off",
    "Stay positive, stay persistent",
    "Perseverance leads to success",
    "Small victories count"
  )

  chosen_message <- sample(encouraging_messages, size = 1)

  chosen_message
}

#' Internal Function to Display a Success Message in the CLI
#'
#' This is an internal function that is not intended for external use. It
#' displays a success message in the Command Line Interface (CLI) using the
#' customized checkthat theme.
#'
#' @noRd
cli_check_success <- function() {
  cli::start_app(theme = checkthat_cli_theme())
  cli::cli_alert_success("all data checks passing")
}

#' Internal Function to Add Encouraging Messages to the CLI
#'
#' This is an internal function that is not intended for external use. It adds
#' encouraging messages to the Command Line Interface (CLI) using the customized
#' checkthat theme.
#'
#' @param msg A custom message to display. If not provided, a random encouraging message will be selected.
#' @param p The probability of displaying the message. Default is 0.33.
#'
#' @noRd
#'
#' @returns (invisibly) The unmodifiied \code{msg} string from above.
cli_add_encouragement <- function(msg = encouraging_message(), p = .33) {
  if (stats::runif(1) < p) {
    cli::start_app(theme = checkthat_cli_theme())
    cli::cli_text("")
    cli::cli_text(msg)
  }

  invisible(msg)
}

#' Internal Function to Trigger a Test Error in the CLI
#'
#' This is an internal function that is not intended for external use. It
#' triggers a test error message in the Command Line Interface (CLI) using the
#' customized checkthat theme.
#'
#' @noRd
cli_throw_test_error <- function() {
  cli::start_app(theme = checkthat_cli_theme())
  cli::cli_text("")
  cli::cli_text("")
  cli::cli_abort("At least one data check failed.")
}

#' Internal Function to Display a Summary of Test Results in the CLI
#'
#' This is an internal function that is not intended for external use. It
#' displays a summary of test results in the Command Line Interface (CLI) using
#' the customized checkthat theme.
#'
#' @param test_results A logical vector representing the results of data checks.
#' @param test_labs A character vector containing labels for the data checks.
#'
#' @noRd
#'
#' @returns (invisibly) A named character vector of check results.
cli_check_summary <- function(test_results, test_labs) {
  if (length(test_results) != length(test_labs)){
    stop("length(test_results) != length(test_labs)")
  }

  cli::start_app(theme = checkthat_cli_theme())
  cli::cli_h1("Data Checks")
  cli::cli_text("")

  for (i in seq_along(test_results)) {
    result <- "{test_labs[i]} --> {test_results[i]}"

    if (test_results[i]) {
      cli::cli_alert_success(result)
    } else {
      cli::cli_alert_danger(result)
    }
  }

  cli::cli_h1("")

  check_summary <- as.list(test_results) |>
    purrr::set_names(test_labs)

  invisible(check_summary)
}

#' Internal Function to Print Data Check Results in the CLI
#'
#' This is an internal function that is not intended for external use. It prints
#' the results of data checks in the Command Line Interface (CLI) using the
#' customized checkthat theme.
#'
#' @param test_results A logical vector representing the results of data checks.
#' @param test_labs A character vector containing labels for the data checks.
#' @param encourage Logical. If \code{TRUE}, encouraging messages will be
#'                  displayed when tests pass.
#'
#' @noRd
cli_print_checks <- function(test_results, test_labs, encourage = TRUE) {
  if (all(test_results) == TRUE) {
    cli_check_success()
  } else {
    cli_check_summary(test_results, test_labs)

    if (encourage) {
      cli_add_encouragement()
    }
  }
}

