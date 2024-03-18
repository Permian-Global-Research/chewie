#' Check if package is installed, if not, abort with message.
#' @noRd
chk_pkg <- function(pkg, abort_fun) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible())
  } else {
    abort_fun()
  }
}


assert_gedi_product <- function(x) {
  if (length(x) > 0 && x %in% c("1B", "2A", "2B", "4A")) {
    return(invisible())
  } else {
    abort_gedi_opts()
  }
}

assert_bool <- function(x) {
  if (isTRUE(x) || isFALSE(x)) {
    return(invisible())
  } else {
    abort_bool(x)
  }
}
