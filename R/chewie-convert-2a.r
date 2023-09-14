l2a_h5_to_dt <- function(beam_id, h5_con) {
    l2a_beam <- h5_con[[beam_id]]

    if ("shot_number" %in% hdf5r::list.datasets(l2a_beam)) {
        if (length(l2a_beam[["rh"]]$dims) == 2) {
            rh <- t(l2a_beam[["rh"]][, ])
        } else {
            rh <- t(l2a_beam[["rh"]][])
        }

        data.table::data.table(
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
    }
}

chewie_convert_2A <- function(h5_src) {
    h5_open <- hdf5r::H5File$new(h5_src$destfile, mode = "r")
    grps <- hdf5r::list.groups(h5_open, recursive = FALSE)
    beam_ids <- grps[startsWith(grps, "BEAM")]


    rh_dt <- purrr::map(beam_ids,
        ~ l2a_h5_to_dt(.x, h5_open),
        .progress = TRUE
    ) |>
        data.table::rbindlist()

    colnames(rh_dt) <- c(
        "beam", "shot_number", "degrade_flag", "quality_flag", "delta_time",
        "sensitivity", "solar_elevation", "lat_lowestmode", "lon_lowestmode",
        "elev_highestreturn", "elev_lowestmode", paste0("rh", seq(0, 100))
    )
    h5_open$close_all()
    return(rh_dt)
}
