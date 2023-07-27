chewie_grab <- function() {
    #---- grab ----
}


#' @title Download GEDI data
#' @description Download GEDI data from the NASA Earthdata in hdf5 format.
#' @param x A chewie.find.x object.
#'
chewie_download <- function(
    x, progress = TRUE, timeout = Inf,
    .dir = getOption("chewie.session.cache")) {
    st_time <- Sys.time()

    #---- download ----

    dl_df <- purrr::map(
        .x = cli::cli_progress_along(
            x$url,
            paste0("Downloading GEDI ", attributes(x)$gedi_product, " data")
        ),
        function(idx = .x) {
            # browser()
            curl::multi_download(
                x$url[idx],
                file.path(.dir, basename(x$url[idx])),
                resume = TRUE,
                progress = FALSE,
                timeout = timeout,
                netrc = TRUE,
                netrc_file = Sys.getenv("CHEWIE_NETRC")
            )
        }
    ) |>
        data.table::rbindlist()

    check_staus_codes(dl_df)

    inform_time(st_time, "Download")

    return(dl_df)
}


check_staus_codes <- function(x) {
    if (any(!x$status_code %in% c(200, 206)) || any(isFALSE(x$success))) {
        log_path <- file.path(
            getOption("chewie.session.cache"),
            paste0("GEDI-dl-log-", dt_snake_case(), ".rds")
        )

        saveRDS(x, log_path)

        rds_quote <- paste0('"', log_path, '"')

        cli::cli_inform(c(
            "x" = "Some Downloads have not completed successfully.",
            "i" = "Saving log to cache. To read the log, use:",
            ">" = chew_bold_mag("readRDS({rds_quote})")
        ))
    }
    return(invisible())
}
