checkthat_cli_theme <- function(variables) {
  theme <- cli::builtin_theme()
  theme[["h1"]][["color"]] <- "grey40"
  theme[[".alert-success"]][["color"]] <- "grey40"
  theme[[".alert-danger"]][["color"]] <- "grey40"
  theme[["body"]][["color"]] <- "grey40"

  theme
}

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

cli_check_success <- function() {
  cli::start_app(theme = checkthat_cli_theme())
  cli::cli_alert_success("all data checks passing")
}

cli_add_encouragement <- function(msg = encouraging_message(), p = .33) {
  if (runif(1) < p) {
    cli::start_app(theme = checkthat_cli_theme())
    cli::cli_text("")
    cli::cli_text(msg)
  }

  invisible(msg)
}

cli_throw_test_error <- function() {
  cli::start_app(theme = checkthat_cli_theme())
  cli::cli_text("")
  cli::cli_text("")
  cli::cli_abort(
    paste(
      "At least one data check failed. Check your tests before moving on.",
      "Note, if needed, you can prevent check_that from halting execution by",
      "setting {.code raise_error = FALSE}."
    )
  )
}

cli_check_summary <- function(test_results, test_labs) {
  assertthat::are_equal(length(test_results), length(test_labs))

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
