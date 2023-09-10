test_that("checkthat_cli_theme returns a modified CLI theme", {
  theme <- checkthat_cli_theme()

  expect_equal(theme[["h1"]][["color"]], "grey40")
  expect_equal(theme[[".alert-success"]][["color"]], "grey40")
  expect_equal(theme[[".alert-danger"]][["color"]], "grey40")
  expect_equal(theme[["body"]][["color"]], "grey40")
})

test_that("encouraging_message returns a message from the list", {
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

  message <- encouraging_message()

  expect_true(message %in% encouraging_messages)
})

cli::test_that_cli("success", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    cli::cli_alert_success("wow")
  })
})

cli::test_that_cli("success", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    cli_check_success()
  })
})
