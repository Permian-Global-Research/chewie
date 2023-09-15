open_gedi <- function(x) {
    bounds <- chewie_bbox(attributes(x)$aoi)
    gedi_prod <- find_gedi_product(x)


    gp <- file.path(
        getOption("chewie.parquet.cache"),
        gedi_prod
    )

    og <- arrow::open_dataset(gp) |>
        dplyr::filter(
            lat_lowestmode >= bounds$ymin,
            lat_lowestmode <= bounds$ymax,
            lon_lowestmode >= bounds$xmin,
            lon_lowestmode <= bounds$xmax,
            swath_id %in% x$id
        ) |>
        dplyr::mutate(date_time = lubridate::as_datetime(delta_time,
            origin = lubridate::ymd_hms("2018-01-01 00:00:00", tz = "UTC")
        )) |>
        dplyr::relocate(date_time, .after = delta_time)

    return(og)
}

handle_points <- function(context, lat, lon) {
    wk::wk_handle(
        wk::xy(
            x = lon,
            y = lat
        ),
        wk::sfc_writer()
    )
}


collect_gedi <- function(x, find) {
    intersect <- attributes(find)$intersects
    gedi_pnts <- dplyr::collect(x)
    gedi_pnts <- gedi_pnts |>
        dplyr::mutate(
            geometry = handle_points(
                lat = lat_lowestmode,
                lon = lon_lowestmode
            )
        ) |>
        sf::st_as_sf(crs = "EPSG:4326")

    if (isTRUE(intersect)) {
        gedi_pnts <- sf::st_filter(gedi_pnts, attributes(find)$aoi)
    }
    return(gedi_pnts)
}
