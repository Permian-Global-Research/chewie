chewie_grab <- function() {
    #---- grab ----
}


#' @title Download GEDI data
#' @description Download GEDI data from the NASA Earthdata in hdf5 format.
#' @param x A chewie.find.x object.
#'
chewie_download <- function(
    x, progress = TRUE, timeout = 7200,
    .dir = getOption("GEDI-h5-cache-temp")) {
    st_time <- Sys.time()
    if (!dir.exists(.dir)) {
        dir.create(.dir, recursive = TRUE)
    }

    #---- download ----

    nfiles <- length(x$url)

    dl_func <- function(.url, id) {
        destination <- file.path(.dir, basename(.url))

        df <- curl::multi_download(
            .url,
            destfiles = destination,
            resume = TRUE,
            timeout = timeout,
            progress = progress,
            netrc = TRUE,
            netrc_file = Sys.getenv("CHEWIE_NETRC")
        )

        colfunc <- ifelse(id == nfiles, chew_bold_green, chew_bold_cyan)
        cli::cli_alert_success(paste0(
            "   Downloaded {cli::qty(id)}file{?s}:",
            colfunc("{id}"),
            "/",
            chew_bold_green("{nfiles}")
        ))

        return(df)
    }

    # on.exit(cli::cli_progress_done())

    cli::cli_alert_info(paste0(
        "Downloading GEDI ",
        attributes(x)$gedi_product
    ))


    dl_df <-
        purrr::imap(
            .x = x$url,
            ~ dl_func(.x, .y),
            .progress = FALSE
        ) |>
        purrr::list_rbind()

    check_staus_codes(dl_df, nfiles)

    inform_time(st_time, "Download")

    return(dl_df)
}


check_staus_codes <- function(x, n) {
    if (any(!x$status_code %in% c(200, 206, 416)) || any(isFALSE(x$success))) {
        log_path <- file.path(
            getOption("chewie.session.cache"),
            paste0("GEDI-dl-log-", dt_snake_case(), ".rds")
        )

        saveRDS(x, log_path)

        cli::cli_inform(c(
            "x" = "Some Downloads have not completed successfully.",
            "i" = "Saving log to cache. To read the log, use:",
            ">" = paste0(
                chew_bold_cyan("readRDS("),
                chew_bold_yel(paste0('"', log_path, '"')),
                chew_bold_cyan(")")
            )
        ))
    }

    completed <- which(x$status_code == 416)
    if (length(completed) > 0) {
        inform_download_completed(length(completed), n)
    }
    return(invisible())
}


l2a_h5_to_dt <- function(beam_id, h5_con) {
    # browser()
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
    h5_open <- hdf5r::H5File$new(h5_src, mode = "r")
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
