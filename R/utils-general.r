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
find_gedi_product <- function(x, simple = TRUE, err = TRUE) {
  prod_long <- attributes(x)$gedi_product
  if (is.null(prod_long)) {
    ifelse(err,
      cli::cli_abort("No GEDI product associated with object `x`."),
      return(NULL)
    )
  }
  g_prod <- sub(".*\\-", "", prod_long)
  assert_gedi_product(g_prod, err = err)
  return(ifelse(simple, g_prod, prod_long))
}

#' @title create a directory if it doesn't exist
#' @noRd
check_n_make_dir <- function(x) {
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
  }
}

#' @title Combine a list of data.tables into a single data.table
#' @param .l A list of data.tables
#' @return A single data.table
#' @noRd
#' @keywords internal
dt_cbindlist <- function(.l) {
  data.table::setDT(
    unlist(.l, recursive = FALSE),
    check.names = TRUE
  )[]
}

#' @title Get the path to either the user or project `.Renviron` file
#' @keywords internal
#' @noRd
find_renviron <- function() {
  # Check the current working directory
  cwd_renviron <- normalizePath(file.path(getwd(), ".Renviron"), mustWork = FALSE)
  if (file.exists(cwd_renviron)) {
    return(cwd_renviron)
  }
  # Check the home directory
  home_renviron <- normalizePath("~/.Renviron", mustWork = FALSE)
  if (file.exists(home_renviron)) {
    return(home_renviron)
  }
  # If neither file exists, return NULL
  return(NULL)
}
