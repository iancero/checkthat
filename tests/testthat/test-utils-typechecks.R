test_that("is_integerlike correctly identifies integer-like values", {
  expect_true(is_integerlike(5))
  expect_true(is_integerlike(5L))
  expect_true(is_integerlike(-5.0))
  expect_true(is_integerlike(0))
  expect_true(is_integerlike(-10))
  expect_true(is_integerlike(1000))
})

test_that("is_integerlike correctly identifies non-integer-like values", {
  expect_false(is_integerlike(5.5))
  expect_false(is_integerlike(1.23))
  expect_false(is_integerlike(-1.23))
  expect_false(is_integerlike("string"))
  expect_false(is_integerlike(c(1, 2, 3)))
})

test_that("is_proportion correctly identifies valid proportion values", {
  expect_true(is_proportion(0))
  expect_true(is_proportion(0.5))
  expect_true(is_proportion(0.75))
  expect_true(is_proportion(1))
  expect_true(is_proportion(1L))
})

test_that("is_proportion correctly identifies non-proportion values", {
  expect_false(is_proportion(1.5))
  expect_false(is_proportion(-0.2))
  expect_false(is_proportion(2))
  expect_false(is_proportion("string"))
  expect_false(is_proportion(c(0.2, 0.3)))
})

test_that("validate_proportion correctly validates valid proportion values", {
  expect_no_error(validate_proportion(0))
  expect_no_error(validate_proportion(0.5))
  expect_no_error(validate_proportion(0.75))
  expect_no_error(validate_proportion(1))
})

test_that("validate_proportion correctly throws an error for non-proportion values", {
  expect_error(validate_proportion(1.5), regexp = "Not a valid proportion")
  expect_error(validate_proportion(-0.2), regexp = "Not a valid proportion")
  expect_error(validate_proportion(2), regexp = "Not a valid proportion")
  expect_error(validate_proportion("string"), regexp = "Not a valid proportion")
  expect_error(validate_proportion(c(0.2, 0.3)), regexp = "Not a valid proportion")
})

test_that("is_count correctly identifies valid count values (including zero)", {
  expect_true(is_count(0))
  expect_true(is_count(5))
  expect_true(is_count(10))
})

test_that("is_count correctly identifies non-count values (including zero)", {
  expect_false(is_count(5.5))
  expect_false(is_count(-10))
  expect_false(is_count("string"))
  expect_false(is_count(c(1, 2, 3)))
})

test_that("is_count correctly identifies valid count values (excluding zero)", {
  expect_true(is_count(1, include_zero = FALSE))
  expect_true(is_count(5, include_zero = FALSE))
  expect_true(is_count(10, include_zero = FALSE))
})

test_that("is_count correctly identifies non-count values (excluding zero)", {
  expect_false(is_count(0, include_zero = FALSE))
  expect_false(is_count(5.5, include_zero = FALSE))
  expect_false(is_count(-10, include_zero = FALSE))
  expect_false(is_count("string", include_zero = FALSE))
  expect_false(is_count(c(1, 2, 3), include_zero = FALSE))
})

test_that("validate_count correctly validates valid count values (including zero)", {
  expect_no_error(validate_count(0))
  expect_no_error(validate_count(5))
  expect_no_error(validate_count(10))
})

test_that("validate_count correctly throws an error for non-count values (including zero)", {
  expect_error(validate_count(5.5), regexp = "Not a valid count value")
  expect_error(validate_count(-10), regexp = "Not a valid count value")
  expect_error(validate_count("string"), regexp = "Not a valid count value")
  expect_error(validate_count(c(1, 2, 3)), regexp = "Not a valid count value")
})

test_that("validate_count correctly validates valid count values (excluding zero)", {
  expect_no_error(validate_count(1, include_zero = FALSE))
  expect_no_error(validate_count(5, include_zero = FALSE))
  expect_no_error(validate_count(10, include_zero = FALSE))
})

test_that("validate_count correctly throws an error for non-count values (excluding zero)", {
  expect_error(validate_count(0, include_zero = FALSE), regexp = "Not a valid count value")
  expect_error(validate_count(5.5, include_zero = FALSE), regexp = "Not a valid count value")
  expect_error(validate_count(-10, include_zero = FALSE), regexp = "Not a valid count value")
  expect_error(validate_count("string", include_zero = FALSE), regexp = "Not a valid count value")
  expect_error(validate_count(c(1, 2, 3), include_zero = FALSE), regexp = "Not a valid count value")
})

test_that("validate_logical_vec correctly validates a valid logical vector", {
  expect_no_error(validate_logical_vec(c(TRUE, FALSE, TRUE)))
  expect_no_error(validate_logical_vec(c(FALSE, TRUE, FALSE)))
  expect_no_error(validate_logical_vec(c(TRUE, TRUE, TRUE)))
  expect_no_error(validate_logical_vec(c(FALSE, FALSE, FALSE)))
})

test_that("validate_logical_vec correctly throws an error for non-logical vectors", {
  expect_error(validate_logical_vec(c(1, 0, 1)), regexp = "logical_vec must be a logical vector")
  expect_error(validate_logical_vec(c("TRUE", "FALSE")), regexp = "logical_vec must be a logical vector")
})

test_that("validate_logical_vec correctly throws an error for empty vectors", {
  expect_error(validate_logical_vec(integer(0)), regexp = "logical_vec must have length of at least 1")
  expect_error(validate_logical_vec(logical(0)), regexp = "logical_vec must have length of at least 1")
  expect_error(validate_logical_vec(character(0)), regexp = "logical_vec must have length of at least 1")
})

test_that("validate_logical_vec correctly warns for all NA values", {
  expect_warning(validate_logical_vec(NA), regexp = "All values of logical_vec are NA")
  expect_warning(validate_logical_vec(c(NA, NA, NA)), regexp = "All values of logical_vec are NA")
})
