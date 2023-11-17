#' @title Convert GEDI 1B hdf data to a data.table
#' @description Internal function for reading GEDI 1B hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @param extra_vals A named list of extra variables to add to the returned data.table.
#' @noRd
l1b_h5_to_dt <- function(beam_id, h5_con, extra_vals) {
  # extract the beam and required vectors
  l1b_beam <- h5_con[[beam_id]]
  all_rx_wav <- l1b_beam[["rxwaveform"]][]

  # we could include txwaveform but it is a lot more work for minimal gain - if
  # there is a clear need for this we could consider adding it.
  if ("txwaveform" %in% extra_vals) {
    extra_vals <- setdiff(extra_vals, "txwaveform")
    no_tx_waveform_warn()
  }

  # we don't want to extract rxwaveform in this way so if given we will remove.
  if ("rxwaveform" %in% extra_vals) {
    extra_vals <- setdiff(extra_vals, "rxwaveform")
  }

  # functions for extracting waveforms for each shot.
  rxwave <- Vectorize(
    function(rx_starts, rx_counts) {
      end_idx <- rx_starts + rx_counts - 1
      compress_waveform(all_rx_wav[rx_starts:end_idx])
    }
  )

  vals <- dt_builder(l1b_beam, colnames_1b, extra_vals) |>
    dt_cbindlist()

  # get the rx waveforms
  data.table::set(vals,
    j = "rxwaveform",
    value = rxwave(
      vals$rx_sample_start_index,
      vals$rx_sample_count
    )
  )

  return(vals)
}


#' @title Convert GEDI 2A hdf data to a data.table
#' @description Internal function for reading GEDI 2A hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @param extra_vals A named list of extra variables to add to the returned data.table.
#' @noRd
l2a_h5_to_dt <- function(beam_id, h5_con, extra_vals) {
  l2a_beam <- h5_con[[beam_id]]

  vals <- dt_builder(l2a_beam, colnames_2a, extra_vals)

  if ("shot_number" %in% hdf5r::list.datasets(l2a_beam)) {
    if (length(l2a_beam[["rh"]]$dims) == 2) {
      rh_matrix <- t(l2a_beam[["rh"]][, ])
    } else {
      rh_matrix <- t(l2a_beam[["rh"]][])
    }
    rh <- data.table::data.table(rh_matrix) |>
      data.table::setnames(paste0("rh", seq(0, 100))) |>
      list()
  } else {
    rh <- NULL
  }

  return(dt_cbindlist(c(vals, rh)))
}

#' @title Convert GEDI 2B hdf data to a data.table
#' @description Internal function for reading GEDI 2B hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @param extra_vals A named list of extra variables to add to the returned data.table.
#' @noRd
l2b_h5_to_dt <- function(beam_id, h5_con, extra_vals) {
  g2b_profiler <- function(.colname, .beam) {
    t(.beam[[.colname]][, 1:.beam[[.colname]]$dims[2]]) |>
      data.table::data.table() |>
      data.table::setnames(
        paste0(.colname, seq(0, 145, 5), "_", seq(5, 150, 5), "m")
      ) |>
      list()
  }

  l2b_beam <- h5_con[[beam_id]]

  vals <- dt_builder(l2b_beam, colnames_2b, extra_vals)

  pavd_z <- g2b_profiler("pavd_z", l2b_beam)

  pai_z <- g2b_profiler("pai_z", l2b_beam)

  return(dt_cbindlist(c(vals, pavd_z, pai_z)))
}

compress_waveform <- function(x) {
  I(as.integer(x * 1e4))
}

#' @title build a data.table from a gedi hdf5 file
#' @description internal function for constructing data.tables from 1D GEDI
#' hdf5 datasets
#' @param .beam A `H5Group` object consituting a beam.
#' @param .f A function to get the column names for a given GEDI product.
#' @param .ev A named list of extra variables to add to the returned data.table.
#' @noRd
dt_builder <- function(.beam, .f, .ev) {
  dt_list <- purrr::imap(
    .f(.ev),
    purrr::safely(function(.x, .y) {
      setNames(data.table::data.table(.beam[[.x]][]), .y)
    })
  ) |>
    purrr::map(purrr::pluck, "result")
  names(dt_list) <- NULL
  return(dt_list)
}

#' @title column names and hdf locations for gedi 1B variables
#' @noRd
colnames_1b <- function(.ev = NULL) {
  l <- c(list(
    degrade = "geolocation/degrade",
    elevation_bin0 = "geolocation/elevation_bin0",
    elevation_lastbin = "geolocation/elevation_lastbin",
    latitude_bin0 = "geolocation/latitude_bin0",
    latitude_lastbin = "geolocation/latitude_lastbin",
    longitude_bin0 = "geolocation/longitude_bin0",
    longitude_lastbin = "geolocation/longitude_lastbin",
    beam = "beam",
    delta_time = "delta_time",
    rx_sample_count = "rx_sample_count",
    rx_sample_start_index = "rx_sample_start_index",
    shot_number = "shot_number"
  ), .ev)
  return(l[!duplicated(unlist(l))])
}

#' @title column names and hdf locations for gedi 2A variables
#' @noRd
colnames_2a <- function(.ev = NULL) {
  l <- c(list(
    beam = "beam",
    degrade_flag = "degrade_flag",
    delta_time = "delta_time",
    elev_highestreturn = "elev_highestreturn",
    elev_lowestmode = "elev_lowestmode",
    lat_lowestmode = "lat_lowestmode",
    lon_lowestmode = "lon_lowestmode",
    quality_flag = "quality_flag",
    sensitivity = "sensitivity",
    shot_number = "shot_number",
    solar_elevation = "solar_elevation"
  ), .ev)
  return(l[!duplicated(unlist(l))])
}

#' @title column names and hdf locations for gedi 2B variables
#' @noRd
colnames_2b <- function(.ev = NULL) {
  l <- c(list(
    algorithmrun_flag = "algorithmrun_flag",
    ancillary = "ancillary",
    beam = "beam",
    channel = "channel",
    cover = "cover",
    delta_time = "delta_time",
    fhd_normal = "fhd_normal",
    degrade_flag = "geolocation/degrade_flag",
    digital_elevation_model = "geolocation/digital_elevation_model",
    elev_highestreturn = "geolocation/elev_highestreturn",
    elev_lowestmode = "geolocation/elev_lowestmode",
    elevation_bin0 = "geolocation/elevation_bin0",
    elevation_bin0_error = "geolocation/elevation_bin0_error",
    elevation_lastbin = "geolocation/elevation_lastbin",
    elevation_lastbin_error = "geolocation/elevation_lastbin_error",
    height_bin0 = "geolocation/height_bin0",
    height_lastbin = "geolocation/height_lastbin",
    lat_highestreturn = "geolocation/lat_highestreturn",
    lat_lowestmode = "geolocation/lat_lowestmode",
    latitude_bin0 = "geolocation/latitude_bin0",
    latitude_bin0_error = "geolocation/latitude_bin0_error",
    latitude_lastbin = "geolocation/latitude_lastbin",
    latitude_lastbin_error = "geolocation/latitude_lastbin_error",
    local_beam_azimuth = "geolocation/local_beam_azimuth",
    local_beam_elevation = "geolocation/local_beam_elevation",
    lon_highestreturn = "geolocation/lon_highestreturn",
    lon_lowestmode = "geolocation/lon_lowestmode",
    longitude_bin0 = "geolocation/longitude_bin0",
    longitude_bin0_error = "geolocation/longitude_bin0_error",
    longitude_lastbin = "geolocation/longitude_lastbin",
    longitude_lastbin_error = "geolocation/longitude_lastbin_error",
    solar_azimuth = "geolocation/solar_azimuth",
    solar_elevation = "geolocation/solar_elevation",
    l2a_quality_flag = "l2a_quality_flag",
    l2b_quality_flag = "l2b_quality_flag",
    land_cover_data = "land_cover_data",
    landsat_treecover = "land_cover_data/landsat_treecover",
    modis_nonvegetated = "land_cover_data/modis_nonvegetated",
    modis_nonvegetated_sd = "land_cover_data/modis_nonvegetated_sd",
    modis_treecover = "land_cover_data/modis_treecover",
    modis_treecover_sd = "land_cover_data/modis_treecover_sd",
    master_frac = "master_frac",
    master_int = "master_int",
    num_detectedmodes = "num_detectedmodes",
    omega = "omega",
    pai = "pai",
    pgap_theta = "pgap_theta",
    pgap_theta_error = "pgap_theta_error",
    rg = "rg",
    rh100 = "rh100",
    rhog = "rhog",
    rhog_error = "rhog_error",
    rhov = "rhov",
    rhov_error = "rhov_error",
    rossg = "rossg",
    rv = "rv",
    selected_l2a_algorithm = "selected_l2a_algorithm",
    selected_rg_algorithm = "selected_rg_algorithm",
    sensitivity = "sensitivity",
    shot_number = "shot_number",
    stale_return_flag = "stale_return_flag",
    surface_flag = "surface_flag"
  ), .ev)
  l[!duplicated(unlist(l))]
}
