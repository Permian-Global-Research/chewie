#' @title open gedi data
#' @description open gedi data as an arrow dataset
#' @param x A chewie.find.x object.
#' @noRd
#' @details
#' This is internal for now - the idea being that when you "grab" GEDI data with
#' `grab_gedi` it is converted to parquet format and saved in the cache. This
#' function is used to open the parquet data at the end of the `grab_gedi`
#' function, reading only the data that is required based on the bounding box
#' and swath IDs of the `chewie.find` object.
#' @return an arrow dataset
open_gedi <- function(x) {
  bounds <- chewie_bbox(attributes(x)$aoi)
  gedi_prod <- find_gedi_product(x)


  gp <- file.path(
    getOption("chewie.parquet.cache"),
    gedi_prod
  )

  add_time <- function(x) {
    x |>
      dplyr::mutate(date_time = lubridate::as_datetime(delta_time,
        origin = lubridate::ymd_hms("2018-01-01 00:00:00", tz = "UTC")
      )) |>
      dplyr::relocate(date_time, .after = delta_time)
  }

  if (gedi_prod == "1B") {
    og <- arrow::open_dataset(gp) |>
      dplyr::filter(
        longitude_bin0 >= bounds$xmin,
        longitude_bin0 <= bounds$xmax,
        latitude_bin0 >= bounds$ymin,
        latitude_bin0 <= bounds$ymax,
        swath_id %in% x$id
      ) |>
      add_time()
  } else {
    og <- arrow::open_dataset(gp) |>
      dplyr::filter(
        lat_lowestmode >= bounds$ymin,
        lat_lowestmode <= bounds$ymax,
        lon_lowestmode >= bounds$xmin,
        lon_lowestmode <= bounds$xmax,
        swath_id %in% x$id
      ) |>
      add_time()
  }

  return(og)
}



#' @title Collect GEDI data into an sf object
#' @description Collect GEDI data, returned from `grab_gedi`, as an sf object.
#' @param x An arrow dataset object.
#' @param gedi_find The chewie.find object used to obtain `x`.
#' @param intersects logical; whether to filter the GEDI data based on the
#' search extent attributed to the `chewie.find` object. Default is to use
#' whatever was specified in chewie.find.
#' @param drop_xy_vars logical; whether to drop the columns used to create the
#' geometry column. Default is `TRUE`.
#' @details
#' This function is used to collect the GEDI data returned from `grab_gedi` as
#' an sf object. It is largely a wrapper for dplyr::collect but converts to sf
#' and filters the gedi footprints based on the search extent attributed to the
#' `chewie.find` object.
#' It is strongly recomended that you make the most of the ability to to edit
#' the gedi data on read by using the `dplyr` verbs before collecting the data.
#' This will save a lot of time and memory. However, make sure that, when
#' selecting columns, you do not remove the `lat_lowestmode` and
#' `lon_lowestmode` columns as these are required to create the geometry column.
#' @return an sf object
#'
#' @examplesIf interactive()
#' prairie_creek <- sf::read_sf(
#'   system.file("geojson", "prairie-creek.geojson", package = "chewie")
#' )
#' prairie_creek_find_4a <- find_gedi(prairie_creek,
#'   gedi_product = "4A",
#'   date_start = "2022-01-01", date_end = "2022-04-01",
#'   cache = FALSE
#' )
#'
#' prairie_creek_grab_4a <- grab_gedi(prairie_creek_find_4a)
#'
#' prairie_creek_4a_sf <- collect_gedi(
#'   prairie_creek_grab_4a,
#'   prairie_creek_find_4a
#' )
#' print(prairie_creek_4a_sf)
#' @export
collect_gedi <- function(
    x, gedi_find,
    intersects = attributes(gedi_find)$intersects,
    drop_xy_vars = TRUE) {
  if ("shot_number" %in% names(x)) {
    # convert shot_number to from Int64 to character; required if saving with sf
    x <- x |>
      dplyr::mutate(shot_number = as.character(shot_number))
  }

  if (find_gedi_product(gedi_find) == "1B") {
    if (!any(c(
      "latitude_bin0", "longitude_bin0",
      "latitude_lastbin", "longitude_lastbin"
    ) %in% names(x))) {
      abort_missing_lon_lat(gedi_find)
    }
    # get the midpoint between the start  and end lat/long of the waveform
    x <- x |>
      dplyr::mutate(
        latitude_avg = (latitude_bin0 + latitude_lastbin) / 2,
        longitude_avg = (longitude_bin0 + longitude_lastbin) / 2,
      )

    lat_col <- "latitude_avg"
    lon_col <- "longitude_avg"
  } else {
    if (!"lat_lowestmode" %in% names(x) || !"lon_lowestmode" %in% names(x)) {
      abort_missing_lon_lat(gedi_find)
    }
    lat_col <- "lat_lowestmode"
    lon_col <- "lon_lowestmode"
  }

  gedi_pnts <- dplyr::collect(x)

  gedi_pnts <- gedi_pnts |>
    dplyr::mutate(
      geometry = handle_points(
        lat = !!rlang::sym(lat_col),
        lon = !!rlang::sym(lon_col)
      )
    ) |>
    sf::st_as_sf(crs = "EPSG:4326")

  if (drop_xy_vars) {
    gedi_pnts <- gedi_pnts |>
      dplyr::select(!dplyr::any_of(c(
        "latitude_avg", "longitude_avg",
        "latitude_bin0", "longitude_bin0",
        "latitude_lastbin", "longitude_lastbin",
        "lat_lowestmode", "lon_lowestmode"
      )))
  }


  if (isTRUE(intersects)) {
    gedi_pnts <- sf::st_filter(gedi_pnts, attributes(gedi_find)$aoi)
  }

  attributes(gedi_pnts)$gedi_product <- find_gedi_product(
    gedi_find,
    simple = FALSE
  )

  attributes(gedi_pnts)$aoi <- attributes(gedi_find)$aoi

  return(gedi_pnts)
}
