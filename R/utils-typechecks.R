is_integerlike <- function(x) {
  if (!is.numeric(x)){
    return(FALSE)
  }

  if (length(x) != 1){
    return(FALSE)
  }

  x == round(x)
}

is_proportion <- function(x) {
  if (!is.numeric(x)){
    return(FALSE)
  }

  if (length(x) != 1){
    return(FALSE)
  }

  (x >= 0) & (x <= 1)
}

validate_proportion <- function(x){
  if(!is_proportion(x)){
    stop('Not a valid proportion')
  }

  TRUE
}

is_count <- function(x, include_zero = TRUE) {
  if (!is_integerlike(x)){
    return(FALSE)
  }

  if (!include_zero){
    return(x > 0)
  }

  x >= 0
}

validate_count <- function(x, include_zero = TRUE){
  if(!is_count(x, include_zero = include_zero)){
    stop('Not a valid count value (i.e., integer of zero or greater).')
  }

  TRUE
}

validate_logical_vec <- function(logical_vec) {
  if (length(logical_vec) < 1) {
    stop("logical_vec must have length of at least 1")
  }

  if (!is.logical(logical_vec)) {
    stop("logical_vec must be a logical vector")
  }

  if (all(is.na(logical_vec))) {
    warning("All values of logical_vec are NA")
  }

  TRUE
}

