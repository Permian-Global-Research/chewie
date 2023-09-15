#' @title Download GEDI data
#' @description internal function to download GEDI data from the NASA Earthdata
#' in hdf5 format, write as parquet, and return a dataframe of the download summary.
#' @param .url A character vector of url to download.
#' @param s_id A character vector of swath id.
#' @param id A numeric indicating the iterator number
#' @param .dir A character vector of the directory to save the hdf5 file.
#' @param timeout A numeric indicating the timeout in seconds.
#' @param progress A logical indicating whether to show a progress bar.
#' @param gedi_prod A character vector indicating the GEDI product.
#' @noRd
chewie_download <- function(
    .url, s_id, id,
    .dir, timeout, progress, gedi_prod, nfiles) {
    destination <- file.path(.dir, basename(.url))

    # here we're using multi_download but only downloading one file at a
    # time. We could do this with other curl functions but this api is nice
    # however the progress bar is not not perfect...
    df <- curl::multi_download(
        .url,
        destfiles = destination,
        resume = TRUE,
        timeout = timeout,
        progress = progress,
        netrc = TRUE,
        netrc_file = Sys.getenv("CHEWIE_NETRC")
    )

    attr(df, "class") <- c(
        paste0("chewie.download.", gedi_prod),
        class(df)
    )


    if (isTRUE(check_status_codes(df))) {
        cli::cli_inform(c("*" = "Converting to data.table"))
        gedi_dt <- chewie_convert(df)
        cli::cli_inform(c("*" = "Writing as parquet file"))
        save_dir <- file.path(
            getOption(
                "chewie.parquet.cache"
            ),
            gedi_prod,
            paste0("swath_id=", s_id)
        )
        if (!dir.exists(save_dir)) {
            dir.create(save_dir, recursive = TRUE)
        }

        arrow::write_parquet(
            dplyr::as_tibble(gedi_dt),
            file.path(
                save_dir,
                paste0(
                    tools::file_path_sans_ext(basename(df$destfile)),
                    ".parquet"
                )
            )
        )
        cli::cli_inform(c("*" = "Removing hdf5 file"))
        unlink(df$destfile)

        # report success
        colfunc <- ifelse(id == nfiles, chew_bold_green, chew_bold_cyan)

        cli::cli_alert_success(paste0(
            "   Downloaded {cli::qty(id)}file{?s}:",
            colfunc("{id}"),
            "/",
            chew_bold_green("{nfiles}")
        ))
    } else {
        cli::cli_alert_danger(paste0(
            "   Error Downloading {cli::qty(id)}file{?s}:",
            colfunc("{id}"),
            "/",
            chew_bold_red("{nfiles}")
        ))
    }

    return(df)
}


#' @title Download GEDI data or access from cahce
#' @description Download GEDI data from the NASA Earthdata in hdf5 format.
#' @param x A chewie.find.x object.
#' @param progress A logical indicating whether to show a progress bar.
#' @param timeout A numeric indicating the timeout in seconds.
#' @export
#' @returns An arrow_dplyr_query object.
#' @details
#' This function is the main handler for gedi data - it checks the cache to see
#' if the required GEDI data are already downloaded, and if not, downloads them
#' from the NASA Earthdata. Once downloaded each file is converted to the
#' parquet format and saved in the cache directory. This saves a huge amount of
#' disk space and enables dynamic reading and filtering of the returned "open"
#' arrow dataset.
grab_gedi <- function(
    x, progress = TRUE, timeout = 7200) {
    .dir <- getOption("chewie.h5.cache")
    st_time <- Sys.time()

    if (!dir.exists(.dir)) {
        dir.create(.dir, recursive = TRUE)
    }

    gedi_product <- find_gedi_product(x)

    # function to control file download and conversion.
    x_to_down <- chewie_scan(x)

    # get the number of files to download for informative messaging later.
    nfiles <- nrow(x_to_down)

    if (nfiles > 0) {
        cli::cli_inform(c(">" = paste0(
            "Downloading {nfiles} ",
            chew_bold_yel(attributes(x)$gedi_product),
            " Data files."
        )))

        dl_df <-
            purrr::pmap(
                .l = list(x_to_down$url, x_to_down$id, 1:nfiles),
                ~ chewie_download(
                    ..1, ..2, ..3,
                    .dir, timeout, progress, gedi_product, nfiles
                )
            ) |>
            purrr::list_rbind()

        log_staus_codes(dl_df, nfiles)
        inform_time(st_time, "Download")
    }

    return(open_gedi(x))
}

check_status_codes <- function(x) {
    if (any(x$status_code %in% c(200, 206, 416)) || any(isFALSE(x$success))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

log_staus_codes <- function(x, n) {
    if (isFALSE(check_status_codes(x))) {
        log_path <- file.path(
            tempdir(),
            paste0("GEDI-dl-log-", dt_snake_case(), ".rds")
        )

        saveRDS(x, log_path)

        cli::cli_abort(c(
            "x" = "Some Downloads have not completed successfully.",
            "i" = "Saving log file. To read the log, use:",
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
