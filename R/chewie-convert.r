#' @title Convert GEDI data to a `chewie` object
#' @description Internal s3 methods for reading GEDI hdf data as a data.table
#'  object.
#' @param x A `chewie.download` object.
#' @param ... arguments passed to `chewie_convert`
#' @noRd
#' @export
#'
chewie_convert <- function(x, ...) {
    UseMethod("chewie_convert")
}

#' @export
chewie_convert.chewie.default <- function(x, ...) {
    cli::cli_abort("Hold Up! No method for converting {class(x)} objects yet!!")
}


#' @export
chewie_convert.chewie.download.2A <- function(x, ...) {
    chewie_convert_2A(x)
}
