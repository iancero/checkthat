sample_data <- data.frame(
  x = c(1, 2, 3),
  y = c("A", "B", "C")
)

test_that("Column 'x' exists", {
  expect_no_error(check_that(sample_data, exists('x')))
})

test_that("Column 'z' does not exist", {
  expect_error(check_that(sample_data, exists('z')))
})

test_that("Mean of column 'x' is 2", {
  expect_no_error(check_that(sample_data, mean(x) == 2))
})

test_that("Number of rows is greater than 0", {
  expect_no_error(check_that(sample_data, nrow(.d) > 0))
})

test_that("Column 'y' contains unique values", {
  expect_no_error(check_that(sample_data, length(unique(y)) == nrow(.d)))
})

test_that("Data frame has 2 columns", {
  expect_no_error(check_that(sample_data, ncol(.d) == 2))
})

test_that("Column 'y' contains valid values", {
  expect_no_error(check_that(sample_data, all(y %in% c("A", "B", "C"))))
})

test_that("Column 'x' has numeric values", {
  expect_no_error(check_that(sample_data, all(is.numeric(x))))
})

test_that("Column 'x' has no missing values", {
  expect_no_error(check_that(sample_data, all(!is.na(x))))
})

test_that("Column 'y' has character values", {
  expect_no_error(check_that(sample_data, all(is.character(y))))
})

test_that("Column 'x' has values > 0", {
  expect_no_error(check_that(sample_data, all(x > 0)))
})

test_that("Column 'y' has values of length 1", {
  expect_no_error(check_that(sample_data, all(nchar(y) == 1)))
})

test_that("Data frame has specific column names", {
  expect_no_error(check_that(sample_data, all(c("x", "y") %in% colnames(.d))))
})

test_that("Column 'x' has all unique values", {
  expect_no_error(check_that(sample_data, length(unique(x)) == length(x)))
})

test_that("Column 'x' has valid levels", {
  expect_no_error(check_that(data.frame(factor_col = factor(c("A", "B", "C"))),
                             all(factor_col %in% c("A", "B", "C"))))
})

test_that("Date column is within range", {
  expect_no_error(check_that(data.frame(date_col = as.Date(c("2023-01-01", "2023-06-30"))),
                             all(date_col >= as.Date("2023-01-01") & date_col <= as.Date("2023-12-31"))))
})

test_that("Time column has valid values", {
  expect_no_error(
    check_that(
      data.frame(
        time_col = as.POSIXct(c("2023-08-19 12:00:00", "2023-08-19 15:30:00"))),
       all(lubridate::hour(time_col) %in% c(12, 15) & lubridate::minute(time_col) %in% c(0, 30))))
})

test_that("Function works within a pipeline", {
  expect_no_error(
    sample_data |>
      dplyr::filter(x > 1) |>
      check_that(mean(x) > 1))
})

# test_that("Function works with grouped data", {
#
#   # TODO: add support for grouped data frames
#
#   expect_no_error(check_that(sample_data |> dplyr::group_by(y) |> check_that(n() == 1)))
# })

test_that("Function works with a mutate operation", {
  expect_no_error(
    sample_data |>
      dplyr::mutate(new_col = x * 2) |>
      check_that(all(new_col == x * 2)))
})

test_that("Function works with summarise operation", {
  expect_no_error(
    sample_data |>
      dplyr::summarise(mean_x = mean(x)) |>
      check_that(mean_x > 0))
})

test_that("Function works with left_join operation", {
  expect_no_error(
    sample_data |>
      dplyr::left_join(
          y = data.frame(
            y = c("A", "C", "B"),
            z = c("apple", "banana", "clementine")),
          by = "y") |>
       check_that(all(!is.na(z))))
})

test_that("Function works with across operation", {
  expect_no_error(
    sample_data |>
      dplyr::mutate(dplyr::across(.cols = x, .fns = ~ .x * 10)) |>
      check_that(all(x >= 10)))
})

test_that("Function works with arrange operation", {
  expect_no_error(
    sample_data |>
      dplyr::arrange(desc(x)) |>
      check_that(x[1] >= x[2] && x[2] >= x[3]))
})

test_that("Function works with filter operation", {
  expect_no_error(
    sample_data |>
      dplyr::filter(x > 1) |>
      check_that(all(x > 1)))
})

test_that("Function works with nested data frames", {
  expect_no_error(sample_data |>
                    tidyr::nest() |>
                    dplyr::mutate(data = purrr::map(data, ~ check_that(.x, all(x <= 3)))) |>
                    tidyr::unnest(data) |>
                    check_that(all(x <= 3)))
})
