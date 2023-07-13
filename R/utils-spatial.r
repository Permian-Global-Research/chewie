#' Bind list of sf objects using data.table
#' @param x list of sf objects
#' @return sf object
#' @noRd
sf_rbindlist <- function(x) {
    if (length(x) == 0) cli::cli_abort("list is empty!")
    geom_name <- attr(x[[1]], "sf_column")
    x <- data.table::rbindlist(x, use.names = FALSE)
    x[[geom_name]] <- sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
    x <- sf::st_as_sf(x)
    sf::st_geometry(x) <- "geometry"
    return(x)
}
