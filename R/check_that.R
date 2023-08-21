check_that <- function(.data, ..., print = TRUE, raise_error = TRUE, encourage = TRUE){

  # TODO: add support for grouped data frames

  dots <- rlang::enquos(...)

  data_mask <- rlang::as_data_mask(.data)
  data_mask$.d <- .data

  test_labs <- purrr::map_chr(dots, .f = rlang::as_label)
  test_results <- purrr::map_lgl(dots, .f = ~ rlang::eval_tidy(.x, data_mask))

  if(print){
    cli_print_checks(test_results, test_labs, encourage = encourage)
  }

  if(raise_error & any(test_results != TRUE)){
    cli_throw_test_error()
  }

  invisible(.data)
}

