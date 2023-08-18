is_integer <- function(x) {
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
  if (!is.numeric(x)){
    return(FALSE)
  }

  if (length(x) != 1){
    return(FALSE)
  }

  if (!is_integer(x)){
    return(FALSE)
  }

  if (!include_zero){
    return(x > 0)
  }

  x >= 0
}

validate_count <- function(x){
  if(!is_count(x)){
    stop('Not a valid count value (i.e., integer of zero or greater).')
  }

  TRUE
}

