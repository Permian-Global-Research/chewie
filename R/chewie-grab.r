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
    if (!dir.exists(.dir)) {
        dir.create(.dir, recursive = TRUE)
    }

    #---- download ----



    dl_func <- function(.url) {
        dest_file <- file.path(.dir, basename(.url))
        # Check if the file already exists
        if (file.exists(dest_file)) {
            # Get the file size of the partially downloaded file
            partial_size <- file.info(dest_file)$size
        } else {
            # If the file doesn't exist, set the partial size to 0
            partial_size <- 0
        }

        gedi_handle <- curl::new_handle(
            timeout = timeout,
            netrc = TRUE,
            netrc_file = Sys.getenv("CHEWIE_NETRC")
        )

        # Download the file using the curl handle
        curl::curl_download(
            url,
            destfile = dest_file,
            handle = curl_handle
        )


        curl::multi_download(
            .url,
            file.path(.dir, basename(.url)),
            resume = TRUE,
            progress = FALSE,
            timeout = timeout,
            netrc = TRUE,
            netrc_file = Sys.getenv("CHEWIE_NETRC")
        )
    }

    if (isTRUE(progress)) {
        progress <- list(
            clear = TRUE,
            format = paste0(
                "{cli::pb_bar} {pb_current_bytes}/{pb_total_bytes} ",
                "[{ansi_trimws(pb_rate_bytes)}]"
            ),
            name = paste0("Downloading GEDI ", attributes(x)$product, " data"),
            type = "download"
        )
    }


    dl_df <- purrr::map(
        .x = x$url,
        ~ dl_func(.x),
        .progress = progress,
    ) |>
        purrr::list_rbind()

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
