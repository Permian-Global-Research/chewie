#' @title Convert GEDI data to a `chewie` object
#' @description Internal s3 methods for reading GEDI hdf data as a data.table
#'  object.
#' @param x A `chewie.download` object.
#' @param ... arguments passed to `chewie_convert`
#' @noRd
#'
chewie_convert <- function(x, ...) {
  UseMethod("chewie_convert")
}

#' @export
chewie_convert.chewie.default <- function(x, ...) {
  cli::cli_abort("Hold Up! No method for converting {class(x)} objects yet!!")
}

#' @export
chewie_convert.chewie.download.1B <- function(x, ...) {
  h5_to_gedi_dt(
    x,
    .f = l1b_h5_to_dt,
    .progress = TRUE
  )
}

#' @export
chewie_convert.chewie.download.2A <- function(x, ...) {
  h5_to_gedi_dt(
    x,
    .f = l2a_h5_to_dt,
    .progress = TRUE
  )
}

#' @export
chewie_convert.chewie.download.2B <- function(x, ...) {
  h5_to_gedi_dt(
    x,
    .f = l2b_h5_to_dt,
    .progress = TRUE
  )
}

#' @export
chewie_convert.chewie.download.4A <- function(x, ...) {
  h5_to_gedi_dt(
    x,
    .f = l4a_h5_to_dt,
    .progress = TRUE
  )
}

#' @title caller function to covert hdf5 files to data.table.
#' @param x A `chewie.download` object.
#' @param .f a function to use to convert `x`
#' @param .progress logical. Should progress bar be displayed?
#' @noRd
#' @keywords internal
h5_to_gedi_dt <- function(x, .f, .progress = TRUE) {
  h5_open <- hdf5r::H5File$new(x$destfile, mode = "r")

  on.exit(h5_open$close())
  grps <- hdf5r::list.groups(h5_open, recursive = FALSE)
  beam_ids <- grps[startsWith(grps, "BEAM")]

  purrr::map(
    beam_ids,
    ~ .f(.x, h5_open),
    .progress = .progress
  ) |>
    data.table::rbindlist()
}
