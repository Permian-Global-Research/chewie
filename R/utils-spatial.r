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

#' @title convert long/lat to sfc points geometry
#' @description wrapper for wk::wk_handle to convert long/lat to sfc points
#' @param lat A numeric vector of latitudes
#' @param lon A numeric vector of longitudes
#' @noRd
#' @returns an sfc POINT geometry vector.
handle_points <- function(lat, lon) {
    wk::wk_handle(
        wk::xy(
            x = lon,
            y = lat
        ),
        wk::sfc_writer()
    )
}


#' @title Get spatial outline of object
#' @description Used internally to create an sfc object of the requesed aoi.
#' @param x object of class `sf`, `spatVector`, `spatRaster`, `sfc`, `stars`,
#' `stars_proxy`, or `numeric`.
#' @return sfc object
#' @noRd
#' @export
get_spat_outline <- function(x) {
    UseMethod("get_spat_outline")
}
#' @noRd
#' @export
get_spat_outline.sfc <- function(x) {
    sf::st_transform(sf::st_union(x), "EPSG:4326")
}
#' @noRd
#' @export
get_spat_outline.sf <- function(x) {
    sf::st_transform(sf::st_union(x), "EPSG:4326")
}
#' @noRd
#' @export
get_spat_outline.SpatVector <- function(x) {
    sf::st_as_sf(x) |>
        sf::st_union() |>
        sf::st_transform("EPSG:4326")
}
#' @noRd
#' @export
get_spat_outline.SpatRaster <- function(x) {
    box_outline(x)
}
#' @noRd
#' @export
get_spat_outline.stars <- function(x) {
    box_outline(x)
}
#' @noRd
#' @export
get_spat_outline.stars_proxy <- function(x) {
    box_outline(x)
}
#' @noRd
#' @export
get_spat_outline.numeric <- function(x) {
    box_outline(x)
}

box_outline <- function(x) {
    chewie_bbox(x) |>
        sf::st_as_sfc()
}



#' @title Get only intersecting swaths for the aoi
#' @description Used internally to get only the intersecting swaths for the
#' requested aoi.
#' @param aoi object of class `sf`, `spatVector`, `spatRaster`, `sfc`, `stars`,
#' `stars_proxy`, or `numeric` representing the area of interest.
#' @param swaths object of class `sf` or `sfc` representing the swaths.
#' @return sf object
#' @noRd
#' @export
get_swath_intersect <- function(aoi, swaths) {
    UseMethod("get_swath_intersect")
}

#' @noRd
#' @export
get_swath_intersect.sfc <- function(aoi, swaths) {
    sf::st_filter(swaths, sf::st_transform(aoi, sf::st_crs(swaths)))
}

#' @noRd
#' @export
get_swath_intersect.sf <- function(aoi, swaths) {
    sf::st_filter(swaths, sf::st_transform(aoi, sf::st_crs(swaths)))
}

#' @noRd
#' @export
get_swath_intersect.SpatVector <- function(swaths, aoi) {
    sf::st_filter(
        swaths,
        sf::st_transform(sf::st_as_sf(aoi), sf::st_crs(swaths))
    )
}

#' @noRd
#' @export
get_swath_intersect.SpatRaster <- function(swaths, aoi) {
    swaths
}

#' @noRd
#' @export
get_swath_intersect.stars <- function(swaths, aoi) {
    swaths
}

#' @noRd
#' @export
get_swath_intersect.stars_proxy <- function(swaths, aoi) {
    swaths
}

#' @noRd
#' @export
get_swath_intersect.numeric <- function(swaths, aoi) {
    swaths
}
