#' @title Show an interactive map of a chewie_* object.
#' @param x object of class `chewie_find` or `chewie_grab`.
#' @param zcol character; name of the column to use for coloring footprints.
#' @param time_group character; if `interactive = TRUE`, group the swaths by
#' this time unit. Must be one of `"year"` or `"month"`.
#' @param alpha numeric; transparency of the swaths.
#' @param pal character; name of the color palette to use for the swaths/footprints.
#' see `hcl.pals()` for a list of available palettes.
#' @param aoi_color character; color of the AOI polygon.
#' @param zoom numeric; zoom level of the map. If `NULL` (default), the zoom
#' level is determined automatically by the extent of the GEDI swaths.
#' @param interactive logical; if `TRUE` (default), return an interactive map.
#'  If `FALSE`, return a static map.
#' @param file character; if `interactive = FALSE`, save the static map to this
#' file.
#' @param ... additional arguments passed to `mapview::mapview`.
#' @details
#' This function is a wrapper around `mapview::mapview()`. It is designed to
#' work with `chewie_find()` and `chewie_grab()` objects.
#'
#' @return interactive leaflet map
#' @rdname chewie_show
#' @export
chewie_show <- function(
    x, ...) {
  chk_pkg("mapview", abort_no_mapview)
  UseMethod("chewie_show")
}

#' @export
chewie_show.default <- function(x, ...) {
  chewie_show_warn(x)
}

#' @rdname chewie_show
#' @export
chewie_show.sf <- function(
    x,
    zcol = NULL,
    alpha = 0.6,
    pal = "Zissou 1",
    aoi_color = "black",
    zoom = NULL,
    interactive = TRUE,
    file = tempfile(fileext = ".png"),
    ...) {
  pal <- rlang::arg_match(pal, grDevices::hcl.pals())
  if (!is.null(zcol)) {
    zcol <- rlang::arg_match(zcol, colnames(x))
  }
  gprod <- find_gedi_product(x, simple = FALSE, err = FALSE)
  if (is.null(gprod)) {
    NextMethod()
  } else {
    if (isFALSE(interactive)) {
      mapview::mapviewOptions(fgb = FALSE)
      on.exit(mapview::mapviewOptions(fgb = TRUE))
    }

    mv_gen(
      x, zcol, gprod, alpha, 0.9, pal, aoi_color, zoom, interactive, file,
      zoom_on_aoi = FALSE, ...
    )
  }
}

#' @rdname chewie_show
#' @export
chewie_show.chewie.find <- function(
    x,
    time_group = c("year", "month"),
    alpha = 0.5,
    pal = "Zissou 1",
    aoi_color = "black",
    zoom = NULL,
    interactive = TRUE,
    file = tempfile(fileext = ".png"),
    ...) {
  time_group <- rlang::arg_match(time_group, c("year", "month"))
  pal <- rlang::arg_match(pal, grDevices::hcl.pals())

  x$time <- switch(time_group[1],
    year = lubridate::year(x$time_start),
    month = lubridate::month(x$time_start)
  )

  mv_gen(
    x, "time", sprintf("GEDI swaths (%s)", time_group[1]),
    alpha, 0, pal, aoi_color, zoom, interactive, file, ...
  )
}

basemap_opts <- function() {
  bm_opts <- mapview::mapviewGetOption("basemaps")
  mapview::mapviewOptions(basemaps = c(
    "OpenStreetMap",
    "CartoDB.Positron",
    "Esri.WorldImagery"
  ))
  return(bm_opts)
}

aoi_mv <- function(x, aoi_color) {
  mapview::mapview(attributes(x)$aoi,
    layer.name = "AOI",
    alpha.regions = 0, color = aoi_color, lwd = 2
  )
}

mv_gen <- function(
    x, zcol, layer_name, alpha, b_alpha, pal,
    aoi_color, zoom, interactive, file, zoom_on_aoi = TRUE, ...) {
  if (isFALSE(interactive)) {
    mapview::mapviewOptions(fgb = FALSE)
    on.exit(mapview::mapviewOptions(fgb = TRUE))
  }

  on.exit(mapview::mapviewOptions(basemaps = basemap_opts()))

  .mv <- aoi_mv(x, aoi_color) +
    mapview::mapview(x,
      layer.name = layer_name,
      zcol = zcol,
      col.regions = grDevices::hcl.colors(
        n = length(unique(x[[zcol]])), palette = pal
      ),
      alpha.regions = alpha,
      alpha = b_alpha,
      ...
    )

  if (!is.null(zoom)) {
    if (zoom_on_aoi) {
      aoi_cent <- attributes(x)$aoi
    } else {
      aoi_cent <- x
    }
    cent <- sf::st_centroid(sf::st_union(aoi_cent)) |>
      wk::as_xy() |>
      as.numeric()

    .mv@map <- leaflet::setView(.mv@map, cent[1], cent[2], zoom = zoom)
  }


  if (interactive) {
    return(.mv)
  } else {
    mapview::mapshot2(.mv, file = file)
    utils::browseURL(file)
    return(file)
  }
}
