#' @title Collect waveform data from GEDI 1B data
#' @description Collect waveform data from GEDI 1B data
#' @param x A tibble or data.frame object returned from `collect_gedi`.
#' @return a tibble containing the waveform data.
#' @details
#' This function is used to collect the waveform data from GEDI 1B data returned
#' from `collect_gedi`. The waveform amplitude is converted to a float value with
#' a precision of 0.0001 (i.e. 4 decimal places) and the elevation is calculated
#' from the `elevation_bin0`, `elevation_lastbin` and `rx_sample_count` columns.
#'
#' This function converts the waveform lists for each footprint to long form and
#' merges all shots into a single tibble. The advantage of this is that it
#' enables the analysis and comparison of multiple plots. If you wish to filter
#' the shots that you want to analyse, simply filter the results of 'collect_gedi'
#' before passing to this function.
#' @examplesIf interactive()
#' prairie_creek <- sf::read_sf(
#'   system.file("geojson", "prairie-creek.geojson", package = "chewie")
#' )
#' prairie_creek_find_1b <- find_gedi(prairie_creek,
#'   gedi_product = "1B",
#'   date_start = "2022-01-01", date_end = "2022-04-01",
#'   cache = FALSE
#' )
#'
#' prairie_creek_grab_1b <- grab_gedi(prairie_creek_find_1b)
#'
#' prairie_creek_1b_sf <- collect_gedi(
#'   prairie_creek_grab_1b,
#'   prairie_creek_find_1b
#' )
#'
#' prairie_creek_waveforms <- collect_waveforms(prairie_creek_1b_sf)
#' print(prairie_creek_waveforms)
#'
#' @export
extract_waveforms <- function(x) {
  if (!"rxwaveform" %in% names(x) ||
    !"shot_number" %in% names(x)) {
    cli::cli_abort(c( # TODO: move this to utils-comms.r
      "x" = "No 'rxwaveform' or 'shot_number' columns found in gedi data.",
      "!" = "The `collect_waveform` function only works for GEDI 1B data.",
      "i" = "make sure you haven't removed these unintentionally using
              `dplyr::select` or similar."
    ))
  }

  xc <- x |>
    dplyr::select(
      shot_number, date_time, elevation_bin0,
      elevation_lastbin, rx_sample_count, rxwaveform
    ) |>
    sf::st_drop_geometry()

  purrr::pmap(
    xc,
    function(shot_number, date_time, elevation_bin0,
             elevation_lastbin, rx_sample_count, rxwaveform) {
      data.table::data.table(
        shot_number = shot_number,
        date_time = date_time,
        rxelevation = rev(seq(
          elevation_lastbin, elevation_bin0,
          (elevation_bin0 - elevation_lastbin) / rx_sample_count
        ))[-1],
        rxwaveform = rxwaveform[] / 1e4
      )
    },
    .progress = TRUE
  ) |>
    data.table::rbindlist() |>
    dplyr::as_tibble()
}
