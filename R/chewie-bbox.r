#' Get spatial extent
#'
#' A class agnostic function to return the bounding extent (i.e. bounding box)
#' of a spatial object.
#'
#' @param x A spatial object, file path or source
#' @param ... Not used
#' @family spatial helpers (class agnostic)
#' @rdname chewie_bbox
#' @return A numeric vector of length 4. Values are returned as:
#' "xmin", "xmax", "ymin", "ymax"
#' @examples
#' f <- system.file("ex/elev.tif", package = "terra")
#' chewie_bbox(terra::rast(f))
#' f2 <- system.file("ex/lux.shp", package = "terra")
#' chewie_bbox(sf::read_sf(f2))
#' @export
chewie_bbox <- function(x, ...) {
  UseMethod("chewie_bbox")
}

#' @rdname chewie_bbox
#'
#' @export
chewie_bbox.SpatRaster <- function(x, ...) {
  terra_ext(x)
}

#' @rdname chewie_bbox
#'
#' @export
chewie_bbox.SpatVector <- function(x, ...) {
  terra_ext(x)
}

#' @rdname chewie_bbox
#'
#' @export
chewie_bbox.sf <- function(x, ...) {
  sf_ext(x)
}

#' @rdname chewie_bbox
#'
#' @export
chewie_bbox.sfc <- function(x, ...) {
  sf_ext(x)
}

#' @rdname chewie_bbox
#'
#' @export
chewie_bbox.stars <- function(x, ...) {
  sf_ext(x)
}

#' @rdname chewie_bbox
#'
#' @export
chewie_bbox.stars_proxy <- function(x, ...) {
  sf_ext(x)
}

#' @rdname chewie_bbox
#'
#' @export
#' @details where x is numeric it should be avector of length for with
#' cordinates orderd as xmin, ymin, xmax, ymax.
chewie_bbox.numeric <- function(x, ...) {
  if (length(x) != 4) {
    abort_numeric_bbox(x)
  }
  c("xmin" = x[1], "ymin" = x[2], "xmax" = x[3], "ymax" = x[4])
}

terra_ext <- function(x) {
  b <- as.vector(terra::ext(x))[c("xmin", "ymin", "xmax", "ymax")]
  bcrs <- terra::crs(x)
  transform_box(b, bcrs)
}

sf_ext <- function(x) {
  b <- sf::st_bbox(x)
  bcrs <- sf::st_crs(x)
  transform_box(b, bcrs)
}

transform_box <- function(.box, .crs) {
  x <- sf::st_bbox(.box) |>
    sf::st_as_sfc()
  x <- sf::st_set_crs(x, .crs)
  sf::st_transform(x, "EPSG:4326") |>
    sf::st_bbox()
}
