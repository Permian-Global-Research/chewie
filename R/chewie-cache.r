#' @title Set GEDI Parquet Cache
#' @param .dir character path to the directory to set as the cache.
#' @param renviron character either 'global', 'local' or path to the directory
#' containing the `.Renviron` file to set the `CHEWIE_PARQUET_CACHE`
#' environment and create the directory. If missing, the default cache
#' directory is used.
#' @rdname chewie-cache
#' @family manage cache
#' @export
chewie_setup_cache <- function(.dir, renviron = "global") {
    if (missing(.dir)) {
        .dir <- file.path(chewie_default_dir(), "GEDI-parquet-cache")
    }

    ovw <- add_env_var("CHEWIE_PARQUET_CACHE", .dir, renviron)

    if (!isTRUE(ovw)) {
        return(invisible())
    }

    .subdir_list <- list(
        "GEDI-1B" = file.path(.dir, "GEDI-1B"),
        "GEDI-2A" = file.path(.dir, "GEDI-2A"),
        "GEDI-2B" = file.path(.dir, "GEDI-2B")
    )

    check_n_make_dir <- function(x) {
        if (!dir.exists(x)) {
            dir.create(x, recursive = TRUE)
        }
    }

    lapply(.subdir_list, check_n_make_dir)


    inform_cache_set_success(.dir)
}

#' @title Unset GEDI Parquet Cache
#' @rdname chewie-cache
#' @family manage cache
#' @export
#' @details This `chewie_unset_cache` function will remove the
#' `CHEWIE_PARQUET_CACHE` environment variable from the `.Renviron` file.
chewie_unset_cache <- function(renviron = "global") {
    remove_env_var("CHEWIE_PARQUET_CACHE", renviron)
}

#' @title Get GEDI Parquet Cache
#' @rdname chewie-cache
#' @family manage cache
#' @export
chewie_get_cache <- function() {
    Sys.getenv("CHEWIE_PARQUET_CACHE", unset = NA)
}
