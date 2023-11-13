#' @title Convert GEDI 2A hdf data to a data.table
#' @description Internal function for reading GEDI 2A hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @noRd
l2a_h5_to_dt <- function(beam_id, h5_con) {
  l2a_beam <- h5_con[[beam_id]]

  if ("shot_number" %in% hdf5r::list.datasets(l2a_beam)) {
    if (length(l2a_beam[["rh"]]$dims) == 2) {
      rh <- t(l2a_beam[["rh"]][, ])
    } else {
      rh <- t(l2a_beam[["rh"]][])
    }

    dt <- data.table::data.table(
      beam = rep(beam_id, length(l2a_beam[["shot_number"]][])),
      shot_number = l2a_beam[["shot_number"]][],
      degrade_flag = l2a_beam[["degrade_flag"]][],
      quality_flag = l2a_beam[["quality_flag"]][],
      quality_flag = l2a_beam[["delta_time"]][],
      sensitivity = l2a_beam[["sensitivity"]][],
      solar_elevation = l2a_beam[["solar_elevation"]][],
      lat_lowestmode = l2a_beam[["lat_lowestmode"]][],
      lon_lowestmode = l2a_beam[["lon_lowestmode"]][],
      elev_highestreturn = l2a_beam[["elev_highestreturn"]][],
      elev_lowestmode = l2a_beam[["elev_lowestmode"]][],
      rh
    )

    colnames(dt) <- c(
      "beam", "shot_number", "degrade_flag", "quality_flag", "delta_time",
      "sensitivity", "solar_elevation", "lat_lowestmode", "lon_lowestmode",
      "elev_highestreturn", "elev_lowestmode", paste0("rh", seq(0, 100))
    )
  }
}

#' @title Convert GEDI 2B hdf data to a data.table
#' @description Internal function for reading GEDI 2B hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @noRd
l2b_h5_to_dt <- function(beam_id, h5_con) {
  l2b_beam <- h5_con[[beam_id]]

  vals <- purrr::imap(
    colnames_2b(),
    purrr::safely(function(.x, .y) {
      setNames(data.table::data.table(l2b_beam[[.x]][]), .y)
    })
  ) |>
    purrr::map(purrr::pluck, "result")

  names(vals) <- NULL



  g2b_names <- function(.col) {
    paste0(.col, seq(0, 145, 5), "_", seq(5, 150, 5), "m")
  }

  pavd_z <- t(l2b_beam[["pavd_z"]][, 1:l2b_beam[["pavd_z"]]$dims[2]]) |>
    data.table::data.table() |>
    data.table::setnames(
      g2b_names("pavd_z")
    )

  pai_z <- t(l2b_beam[["pai_z"]][, 1:l2b_beam[["pai_z"]]$dims[2]]) |>
    data.table::data.table() |>
    data.table::setnames(
      g2b_names("pai_z")
    )

  data.table::setDT(
    unlist(
      c(vals, list(pavd_z), list(pai_z)),
      recursive = FALSE
    ),
    check.names = TRUE
  )[]
}


colnames_2b <- function() {
  list(
    "algorithmrun_flag" = "algorithmrun_flag",
    "ancillary" = "ancillary",
    "beam" = "beam",
    "channel" = "channel",
    "cover" = "cover",
    "delta_time" = "delta_time",
    "fhd_normal" = "fhd_normal",
    "degrade_flag" = "geolocation/degrade_flag",
    "digital_elevation_model" = "geolocation/digital_elevation_model",
    "elev_highestreturn" = "geolocation/elev_highestreturn",
    "elev_lowestmode" = "geolocation/elev_lowestmode",
    "elevation_bin0" = "geolocation/elevation_bin0",
    "elevation_bin0_error" = "geolocation/elevation_bin0_error",
    "elevation_lastbin" = "geolocation/elevation_lastbin",
    "elevation_lastbin_error" = "geolocation/elevation_lastbin_error",
    "height_bin0" = "geolocation/height_bin0",
    "height_lastbin" = "geolocation/height_lastbin",
    "lat_highestreturn" = "geolocation/lat_highestreturn",
    "lat_lowestmode" = "geolocation/lat_lowestmode",
    "latitude_bin0" = "geolocation/latitude_bin0",
    "latitude_bin0_error" = "geolocation/latitude_bin0_error",
    "latitude_lastbin" = "geolocation/latitude_lastbin",
    "latitude_lastbin_error" = "geolocation/latitude_lastbin_error",
    "local_beam_azimuth" = "geolocation/local_beam_azimuth",
    "local_beam_elevation" = "geolocation/local_beam_elevation",
    "lon_highestreturn" = "geolocation/lon_highestreturn",
    "lon_lowestmode" = "geolocation/lon_lowestmode",
    "longitude_bin0" = "geolocation/longitude_bin0",
    "longitude_bin0_error" = "geolocation/longitude_bin0_error",
    "longitude_lastbin" = "geolocation/longitude_lastbin",
    "longitude_lastbin_error" = "geolocation/longitude_lastbin_error",
    "solar_azimuth" = "geolocation/solar_azimuth",
    "solar_elevation" = "geolocation/solar_elevation",
    "l2a_quality_flag" = "l2a_quality_flag",
    "l2b_quality_flag" = "l2b_quality_flag",
    "land_cover_data" = "land_cover_data",
    "landsat_treecover" = "land_cover_data/landsat_treecover",
    "modis_nonvegetated" = "land_cover_data/modis_nonvegetated",
    "modis_nonvegetated_sd" = "land_cover_data/modis_nonvegetated_sd",
    "modis_treecover" = "land_cover_data/modis_treecover",
    "modis_treecover_sd" = "land_cover_data/modis_treecover_sd",
    "master_frac" = "master_frac",
    "master_int" = "master_int",
    "num_detectedmodes" = "num_detectedmodes",
    "omega" = "omega",
    "pai" = "pai",
    "pgap_theta" = "pgap_theta",
    "pgap_theta_error" = "pgap_theta_error",
    "rg" = "rg",
    "rh100" = "rh100",
    "rhog" = "rhog",
    "rhog_error" = "rhog_error",
    "rhov" = "rhov",
    "rhov_error" = "rhov_error",
    "rossg" = "rossg",
    "rv" = "rv",
    "selected_l2a_algorithm" = "selected_l2a_algorithm",
    "selected_rg_algorithm" = "selected_rg_algorithm",
    "sensitivity" = "sensitivity",
    "shot_number" = "shot_number",
    "stale_return_flag" = "stale_return_flag",
    "surface_flag" = "surface_flag"
  )
}
