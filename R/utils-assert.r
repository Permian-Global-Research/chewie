#' Check if package is installed, if not, abort with message.
#' @noRd
chk_pkg <- function(pkg, abort_fun) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible())
  } else {
    abort_fun()
  }
}

#' assertion logic for GEDI product
#' @noRd
#' @param x character GEDI product
#' @param err logical whether to throw an error if the product is invalid.
#' @keywords internal
assert_gedi_product <- function(x, err = TRUE) {
  if (length(x) > 0 && x %in% c("1B", "2A", "2B", "4A")) {
    return(invisible())
  } else {
    ifelse(err, abort_gedi_opts(), NULL)
  }
}

#' assertion logic for boolean
#' @noRd
#' @param x logical (or not)
#' @keywords internal
assert_bool <- function(x) {
  if (isTRUE(x) || isFALSE(x)) {
    return(invisible())
  } else {
    x_lab <- deparse(substitute(x))
    abort_bool(x_lab)
  }
}
