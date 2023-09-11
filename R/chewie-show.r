#' @title Show an interactive map of a chewie_* object.
#' @param x object of class `chewie_find` or `chewie_grab`.
#' @param ... additional arguments passed to `mapview::mapview`.
#' @param interactive logical; if `TRUE` (default), return an interactive map.
#'  If `FALSE`, return a static map.
#' @param file character; if `interactive = FALSE`, save the static map to this
#' file.
#' @param time_group character; if `interactive = TRUE`, group the swaths by
#' this time unit. Must be one of `"year"` or `"month"`.
#' @param alpha numeric; transparency of the swaths.
#' @param swath_pal character; name of the color palette to use for the swaths.
#' see `hcl.pals()` for a list of available palettes.
#' @param aoi_color character; color of the AOI polygon.
#' @details
#' This function is a wrapper around `mapview::mapview()`. It is designed to
#' work with `chewie_find()` and `chewie_grab()` objects.
#'
#' @return interactive leaflet map
#' @export
chewie_show <- function(x, ...) {
    chk_pkg("mapview", abort_no_mapview)
    UseMethod("chewie_show")
}

#' @export
chewie_show.default <- function(x, ...) {
    chewie_show_warn(x)
}

#' @export
chewie_show.chewie.find <- function(
    x,
    time_group = c("year", "month"),
    alpha = 0.5,
    swath_pal = "Zissou 1",
    aoi_color = "black",
    interactive = TRUE,
    file = tempfile(fileext = ".png")) {
    if (isFALSE(interactive)) {
        mapview::mapviewOptions(fgb = FALSE)
        on.exit(mapview::mapviewOptions(fgb = TRUE))
    }

    bm_opts <- basemap_opts()

    on.exit(mapview::mapviewOptions(basemaps = bm_opts))

    x$time <- switch(time_group[1],
        year = lubridate::year(x$time_start),
        month = lubridate::month(x$time_start)
    )

    .mv <- mapview::mapview(attributes(x)$aoi,
        layer.name = "AOI",
        alpha.regions = 0, color = aoi_color, lwd = 2
    ) +
        mapview::mapview(x,
            layer.name = sprintf("GEDI swaths (%s)", time_group[1]),
            zcol = "time",
            col.regions = grDevices::hcl.colors(
                n = length(unique(x$time)), palette = swath_pal
            ),
            alpha.regions = alpha,
            alpha = 0
        )


    if (interactive) {
        return(.mv)
    } else {
        mapview::mapshot2(.mv, file = file)
        utils::browseURL(file)
        return(file)
    }
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
