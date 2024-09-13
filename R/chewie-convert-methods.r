#' @title Convert GEDI 1B hdf data to a data.table
#' @description Internal function for reading GEDI 1B hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @noRd
l1b_h5_to_dt <- function(beam_id, h5_con) {
  l1b_beam <- h5_con[[beam_id]]

  vals <- dt_builder(
    l1b_beam,
    colnames_generic(l1b_beam,
      drop_cols = c(
        "txwaveform", "rxwaveform", "surface_type"
      ),
      drop_groups = "ancillary"
    )
  )

  bind_vals <- dt_cbindlist(vals)

  add_waveform(l1b_beam, bind_vals, "rxwaveform", compress = TRUE)

  geo_grp <- hdf5r::openGroup(l1b_beam, "geolocation")

  surface_type <- data.table::data.table(geo_grp[["surface_type"]][, ])
  data.table::setnames(
    surface_type,
    paste0(
      "surface_type_",
      c("land", "ocean", "sea_ice", "land_ice", "inland_water")
    )
  )

  return(cbind(bind_vals, surface_type))
}


#' @title Convert GEDI 2A hdf data to a data.table
#' @description Internal function for reading GEDI 2A hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @noRd
l2a_h5_to_dt <- function(beam_id, h5_con) {
  l2a_beam <- h5_con[[beam_id]]

  drp_grps <- c(
    "ancillary",
    "geolocation",
    "rx_1gaussfit", "rx_1gaussfit/ancillary",
    "rx_assess", "rx_assess/ancillary",
    paste0("rx_processing_a", 1:6),
    paste0("rx_processing_a", 1:6, "/ancillary")
  )


  vals <- dt_builder(
    l2a_beam,
    colnames_generic(
      l2a_beam,
      drop_cols = c(
        "rh",
        paste0("elevs_allmodes_a", 1:6),
        paste0("lats_allmodes_a", 1:6),
        paste0("lons_allmodes_a", 1:6),
        paste0("rh_a", 1:6)
      ),
      drop_groups = drp_grps
    )
  )

  rh <- construct_101_df(l2a_beam, "rh", sep = "")


  # --- I think these are unnecessary for now --- TODO: discuss
  # geo_grp <- hdf5r::openGroup(l2a_beam, "geolocation")
  # rha_n <- purrr::map(
  #   paste0("rh_a", 1:6),
  #   ~ construct_101_df(geo_grp, .x)
  # )
  # mode_list <- purrr::map(
  #   c("elevs_allmodes_a", "lats_allmodes_a", "lons_allmodes_a"),
  #   ~ purrr::map(
  #     paste0(.x, 1:6),
  #     ~ construct_modes_df(geo_grp, .x)
  #   )
  # ) |>
  #   purrr::flatten()
  # --------------------------------------------

  rx_cum_list <- paste0("rx_processing_a", 1:6) |>
    purrr::map(
      ~ hdf5r::openGroup(l2a_beam, .x)
    ) |>
    purrr::imap(
      ~ construct_101_df(.x, "rx_cumulative", name_suffix = paste0("a", .y))
    )

  dt_list <- c(
    vals, list(rh), rx_cum_list
    # , rha_n, mode_list, # comment out for now
  )

  return(dt_cbindlist(dt_list))
}

#' @title Convert GEDI 2B hdf data to a data.table
#' @description Internal function for reading GEDI 2B hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' data.table.
#' @noRd
l2b_h5_to_dt <- function(beam_id, h5_con) {
  l2b_beam <- h5_con[[beam_id]]

  vals <- dt_builder(
    l2b_beam,
    colnames_generic(
      l2b_beam,
      drop_cols = c("cover_z", "pai_z", "pavd_z", "pgap_theta_z"),
      drop_groups = c("rx_processing")
    )
  )

  ancil_grp <- vals$ancillary

  z_vars <- c("cover_z", "pai_z", "pavd_z") |>
    purrr::map(
      ~ g2b_profiler(.x, l2b_beam, ancil_grp$dz, ancil_grp$maxheight_cuttoff)
    )

  add_waveform(l2b_beam, vals$beam, "pgap_theta_z", compress = FALSE)

  # build final data.table
  vals$ancillary <- NULL # remove ancillary group from vals
  comb_list <- c(vals, z_vars)

  return(dt_cbindlist(comb_list))
}


#' @title Convert GEDI 4A hdf data to a data.table
#' @description Internal function for reading GEDI 4A hdf data as a data.table
#' object.
#' @param beam_id A character string of the beam id to read.
#' @param h5_con A `H5File` object.
#' @noRd
#' @details
#' We are ignoring the agb_prediction group for now. This seems like a lot of
#' extra data for minimal gain but we can consider retaining it...
l4a_h5_to_dt <- function(beam_id, h5_con) {
  l4a_beam <- h5_con[[beam_id]]

  xvar <- data.table::data.table(t(l4a_beam[["xvar"]][, ]))

  xvar <- construct_modes_df(l4a_beam, "xvar", type = "pred")

  vals <- dt_builder(
    l4a_beam,
    colnames_generic(l4a_beam,
      drop_cols = c("xvar"),
      drop_groups = c("agbd_prediction", "geolocation")
    )
  ) |> dt_cbindlist()

  return(cbind(vals, xvar))
}

#' @title profile GEDI 2B data
#' @description internal function for profiling GEDI 2B data
#' @param .colname A character string of the column name to extract.
#' @param .beam A H5Group object.
#' @param dz A numeric value of the height increment.
#' @param maxz A numeric value of the maximum height.
#' @noRd
#' @keywords internal
g2b_profiler <- function(.colname, .beam, dz, maxz) {
  var_dims <- .beam[[.colname]]$dims
  t(.beam[[.colname]][, 1:var_dims[2]]) |>
    data.table::data.table() |>
    data.table::setnames(
      paste0(.colname, seq(0, maxz - dz, dz), "_", seq(dz, maxz, dz), "m")
    )
}

#' @title convert 2D matrix to a data.table
#' @description internal function for converting a 2D matrix to a data.table
#' @param beam A H5Group object.
#' @param col_name A character string of the column name to extract.
#' @noRd
#' @keywords internal
matrix_2d_as_dt <- function(beam, col_name) {
  t_mat <- t(beam[[col_name]][, ])
  data.table::data.table(t_mat)
}

#' @title construct a dataframe of modes from a GEDI hdf5 dataset
#' @param beam A H5Group object.
#' @param col_name A character string of the column name to extract.
#' @noRd
#' @keywords internal
construct_modes_df <- function(beam, col_name, type = "mode") {
  df_mode <- matrix_2d_as_dt(beam, col_name)
  data.table::setnames(
    df_mode,
    paste(col_name, type, seq(1, ncol(df_mode)), sep = "_")
  )
  return(df_mode)
}

#' Construct a dataframe of 101 columns from a GEDI 101 hdf5 dataset
#' @param beam A H5Group object.
#' @param col_name A character string of the column name to extract.
#' @noRd
#' @keywords internal
construct_101_df <- function(beam, col_name, name_suffix = NULL, sep = "_") {
  df101 <- matrix_2d_as_dt(beam, col_name)
  if (is.null(name_suffix)) {
    name_vec <- paste(col_name, seq(0, 100), sep = sep)
  } else {
    name_vec <- paste(col_name, name_suffix, seq(0, 100), sep = sep)
  }
  data.table::setnames(df101, name_vec)
  return(df101)
}

#' @title add a 1d array or waveform to a data.table as a list column
#' @description internal function for adding a 1D array to a data.table as a
#' list column.
#' @param beam A H5Group object.
#' @param dt A data.table object.
#' @param arr1d A numeric vector.
#' @param id A character string of the column name to add.
#' @param compress A logical indicating whether to compress the waveform data.
#' @noRd
#' @keywords internal
add_waveform <- function(beam, dt, id, compress = TRUE) {
  arr1d <- beam[[id]][]
  rxwave <- Vectorize(
    function(rx_st, rx_co) {
      end_idx <- rx_st + rx_co - 1
      if (compress) {
        return(I(as.integer(arr1d[rx_st:end_idx] * 1e4)))
      } else {
        return(I(as.integer(arr1d[rx_st:end_idx])))
      }
    }
  )

  # get the rx waveforms
  data.table::set(dt,
    j = id,
    value = rxwave(
      dt$rx_sample_start_index,
      dt$rx_sample_count
    )
  )
}


#' @title build a data.table from a gedi hdf5 file
#' @description internal function for constructing data.tables from 1D GEDI
#' hdf5 datasets
#' @param .beam A `H5Group` object consituting a beam.
#' @param .l a named list of groups containing the datasets to extract.
#' @noRd
dt_builder <- function(.beam, .l) {
  purrr::imap(
    .l,
    function(.x, .y) {
      purrr::map(
        .x,
        function(.x) {
          if (.y == "beam") {
            return(setNames(data.table::data.table(.beam[[.x]][]), .x))
          } else {
            open_grp <- hdf5r::openGroup(.beam, .y)
            return(setNames(data.table::data.table(open_grp[[.x]][]), .x))
          }
        }
      )
    }
  ) |>
    purrr::map(
      dt_cbindlist
    )
}

#' @title column names and hdf locations for gedi 4A variables
#' @param .ev A `H5File` object.
#' @param drop_cols A character vector of columns to drop.
#' @param drop_groups A character vector of groups to drop.
#' @noRd
#' @details here the .ev argument is the opened hdf5 file itself. and any
#' extra variables are ignored. This is because this will return all available
#' variables in the hdf5 file.
colnames_generic <- function(.ev, drop_cols = NULL, drop_groups = NULL) {
  grps <- hdf5r::list.groups(.ev)

  grps <- setdiff(grps, drop_groups)

  grp_data <- purrr::map(grps, ~ hdf5r::openGroup(.ev, .x)) |>
    purrr::map(hdf5r::list.datasets) |>
    purrr::set_names(grps)

  main <- list(hdf5r::list.datasets(.ev, recursive = FALSE)) |>
    purrr::set_names("beam")

  all_dat <- c(main, grp_data)

  if (!is.null(drop_cols)) {
    all_dat <- purrr::map(all_dat, ~ setdiff(.x, drop_cols))
  }

  return(all_dat)
}
