chewie_setup_parquet_cache <- function(.dir, renviron = "global") {
    if (missing(.dir)) {
        .dir <- file.path(chewie_default_dir(), "GEDI-parquet-cache")
    }

    if (!dir.exists(.dir)) {
        dir.create(.dir, recursive = TRUE)
    }

    add_env_var("CHEWIE_PARQUET_CACHE", .dir, renviron)
    inform_cache_set_success(.dir)
}


chewie_unset_cache <- function(renviron = "global") {
    remove_env_var("CHEWIE_PARQUET_CACHE", renviron)
}
