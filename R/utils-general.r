#' @title Create a snake case timestamp
#' @noRd
dt_snake_case <- function() {
  sub("\\..*", "",
    x = gsub(":", "-",
      x = gsub(" ", "-", Sys.time())
    )
  )
}

#' @title get the GEDI product from a `chewie.find` object
#' @noRd
find_gedi_product <- function(x) {
  g_prod <- sub(".*\\-", "", attributes(x)$gedi_product)
  assert_gedi_product(g_prod)
  return(g_prod)
}

#' @title create a directory if it doesn't exist
#' @noRd
check_n_make_dir <- function(x) {
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
  }
}

dt_cbindlist <- function(.l) {
  data.table::setDT(
    unlist(.l, recursive = FALSE),
    check.names = TRUE
  )[]
}
