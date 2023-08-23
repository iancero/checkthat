# Sample operators for testing
greater_than_operator <- function(e1, e2) e1 > e2
less_than_operator <- function(e1, e2) e1 < e2
less_than_equal_operator <- function(e1, e2) e1 <= e2
greater_than_equal_operator <- function(e1, e2) e1 >= e2
equal_to_operator <- function(e1, e2) e1 == e2

test_that("Aggregator returns proportion correctly", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec, type = "p")
  expect_equal(result, 0.6)
})

test_that("Aggregator returns count correctly", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec, type = "n")
  expect_equal(result, 3)
})

test_that("Aggregator handles NA values correctly (proportions)", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec_with_na, type = "p", na.rm = TRUE)
  expect_equal(result, 0.50)
})

test_that("Aggregator handles NA values correctly (counts)", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- aggregator(logical_vec_with_na, type = "n", na.rm = TRUE)
  expect_equal(result, 2)
})

test_that("Aggregator throws error for invalid type", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  expect_error(aggregator(logical_vec, type = "invalid_type"))
})

test_that("Aggregator throws error for invalid type length", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  expect_error(aggregator(logical_vec, type = c("p", "n")))
})

test_that("Quantifier with greater_than_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  result <- quant_func(logical_vec, p = 0.5)
  expect_true(result)
})

test_that("Quantifier with greater_than_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  result <- quant_func(logical_vec, n = 2)
  expect_true(result)
})

test_that("Quantifier with less_than_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_operator)
  result <- quant_func(logical_vec, p = 0.7)
  expect_true(result)
})

test_that("Quantifier with less_than_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_operator)
  result <- quant_func(logical_vec, n = 3)
  expect_false(result)
})

test_that("Quantifier throws error for missing p and n", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  expect_error(quant_func(logical_vec))
})

test_that("Quantifier throws error for both p and n", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_operator)
  expect_error(quant_func(logical_vec, p = 0.6, n = 3))
})

test_that("Quantifier with less_than_equal_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_equal_operator)
  result <- quant_func(logical_vec, p = 0.7)
  expect_true(result)
})

test_that("Quantifier with less_than_equal_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(less_than_equal_operator)
  result <- quant_func(logical_vec, n = 3)
  expect_true(result)
})

test_that("Quantifier with greater_than_equal_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_equal_operator)
  result <- quant_func(logical_vec, p = 0.4)
  expect_true(result)
})

test_that("Quantifier with greater_than_equal_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(greater_than_equal_operator)
  result <- quant_func(logical_vec, n = 2)
  expect_true(result)
})

test_that("Quantifier with equal_to_operator works correctly (proportions)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(equal_to_operator)
  result <- quant_func(logical_vec, p = 0.6)
  expect_true(result)
})

test_that("Quantifier with equal_to_operator works correctly (counts)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  quant_func <- quantifier(equal_to_operator)
  result <- quant_func(logical_vec, n = 3)
  expect_true(result)
})

test_that("Proportion calculation works correctly (without NA removal)", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- prop(logical_vec)
  expect_equal(result, 0.6)
})

test_that("Proportion calculation works correctly (with NA removal)", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- prop(logical_vec_with_na, na.rm = TRUE)
  expect_equal(result, 0.50)
})

test_that("Proportion calculation handles all TRUE values", {
  all_true_logical_vec <- c(TRUE, TRUE, TRUE)
  result <- prop(all_true_logical_vec)
  expect_equal(result, 1)
})

test_that("Proportion calculation handles all FALSE values", {
  all_false_logical_vec <- c(FALSE, FALSE, FALSE)
  result <- prop(all_false_logical_vec)
  expect_equal(result, 0)
})

test_that("Proportion calculation handles NA values", {
  logical_vec_with_na <- c(TRUE, NA, FALSE, TRUE, FALSE)
  result <- prop(logical_vec_with_na, na.rm = T)
  expect_equal(result, 0.5)
})

test_that("is_p_or_n returns 'p' for valid proportion", {
  result <- is_p_or_n(0.5)
  expect_equal(result, "p")
})

test_that("is_p_or_n returns 'n' for valid count", {
  result <- is_p_or_n(10)
  expect_equal(result, "n")
})

test_that("is_p_or_n throws error for value of 1 (error_on_1 = TRUE)", {
  expect_error(is_p_or_n(1))
})

test_that("is_p_or_n returns 'p' for value of 1 (error_on_1 = FALSE)", {
  result <- is_p_or_n(1, error_on_1 = FALSE)
  expect_equal(result, "p")
})

test_that("is_p_or_n throws error for invalid value", {
  expect_error(is_p_or_n("invalid_value"))
})

test_that("is_p_or_n returns FALSE for invalid value (error_on_nothing = FALSE)", {
  result <- is_p_or_n("invalid_value", error_on_nothing = FALSE)
  expect_false(result)
})

test_that("is_p_or_n throws error for non-numeric value (error_on_nothing = TRUE)", {
  expect_error(is_p_or_n("non_numeric_value"))
})

test_that("is_logical_vec returns TRUE for valid logical vector", {
  result <- is_logical_vec(c(TRUE, FALSE, TRUE))
  expect_true(result)
})

test_that("is_logical_vec returns FALSE for empty vector", {
  result <- is_logical_vec(c())
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for non-logical vector", {
  result <- is_logical_vec(c(1, 2, 3))
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for all NA vector", {
  result <- is_logical_vec(c(NA, NA, NA))
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for mixed vector with NA", {
  result <- is_logical_vec(c(TRUE, 1, "text", NA))
  expect_false(result)
})

test_that("is_logical_vec returns FALSE for non-vector input", {
  expect_true(is_logical_vec(T))
  expect_false(is_logical_vec('a'))
})

test_that("at_least works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_least(logical_vec, p = 0.4)
  expect_true(result)
})

test_that("at_least works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_least(logical_vec, n = 2)
  expect_true(result)
})

test_that("more_than works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- more_than(logical_vec, p = 0.6)
  expect_false(result)
})

test_that("more_than works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- more_than(logical_vec, n = 4)
  expect_false(result)
})

test_that("at_most works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_most(logical_vec, p = 0.6)
  expect_true(result)
})

test_that("at_most works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- at_most(logical_vec, n = 3)
  expect_true(result)
})

test_that("less_than works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- less_than(logical_vec, p = 0.4)
  expect_false(result)
})

test_that("less_than works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- less_than(logical_vec, n = 2)
  expect_false(result)
})

test_that("exactly_equal works correctly with proportion", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- exactly_equal(logical_vec, p = 0.6)
  expect_true(result)
})

test_that("exactly_equal works correctly with count", {
  logical_vec <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  result <- exactly_equal(logical_vec, n = 3)
  expect_true(result)
})

