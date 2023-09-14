read_bbox_area <- function(x, intersect = TRUE) {
    bounds <- chewie_bbox(attributes(x)$aoi)
    gedi_prod <- find_gedi_product(x)

    gp <- file.path(
        getOption("chewie.parquet.cache"),
        gedi_prod
    )

    pnts_dt <- arrow::open_dataset(gp) |>
        dplyr::filter(
            lat_lowestmode >= bounds$ymin,
            lat_lowestmode <= bounds$ymax,
            lon_lowestmode >= bounds$xmin,
            lon_lowestmode <= bounds$xmax
        ) |>
        dplyr::mutate(date_time = lubridate::as_datetime(delta_time,
            origin = lubridate::ymd_hms("2018-01-01 00:00:00", tz = "UTC")
        )) |>
        dplyr::collect()

    pnts_dt_sf <- dplyr::mutate(pnts_dt,
        geometry =
            wk::wk_handle(
                wk::xy(
                    x = pnts_dt$lon_lowestmode,
                    y = pnts_dt$lat_lowestmode
                ),
                wk::sfc_writer()
            )
    ) |>
        sf::st_as_sf(crs = "EPSG:4326")

    if (isTRUE(intersect)) {
        pnts_dt_sf <- sf::st_filter(pnts_dt_sf, attributes(x)$aoi)
    }
    return(pnts_dt_sf)
}
