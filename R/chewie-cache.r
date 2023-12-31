#' @title Set GEDI Parquet Cache
#' @param .dir character path to the directory that should contain the cache.
#' @param renviron character either 'user', '"project"' or path to the directory
#' containing the `.Renviron` file to set the `CHEWIE_PARQUET_CACHE`
#' environment and create the directory. If missing, the default cache
#' directory is used.
#' @rdname chewie-cache
#' @family manage cache
#' @export
chewie_setup_cache <- function(
    .dir = chewie_default_dir(),
    renviron = "user",
    quiet = FALSE) {
  dir_parquet <- file.path(.dir, "GEDI-parquet-cache")
  dir_h5 <- file.path(.dir, "GEDI-h5-cache-temp")
  di_find_gedi <- file.path(.dir, "find-gedi-cache")

  ovw <- add_env_var("CHEWIE_CACHE_HOME", .dir, renviron)
  if (!isTRUE(ovw)) {
    return(invisible())
  }

  check_n_make_dir(dir_parquet)
  check_n_make_dir(dir_h5)
  check_n_make_dir(di_find_gedi)

  chewie_set_cache_opts()


  if (!quiet) {
    inform_cache_set_success(.dir)
  }
  return(invisible())
}

#' @title Unset GEDI Cache
#' @rdname chewie-cache
#' @family manage cache
#' @export
#' @details This `chewie_unset_cache` function will remove the
#' `CHEWIE_PARQUET_CACHE` environment variable from the `.Renviron` file.
chewie_unset_cache <- function(renviron = "user") {
  cli::cli_inform(
    paste0(
      chew_bold_mag("?"),
      paste0(
        "   Do you really want to unset your GEDI cache environment
                variable?"
      )
    )
  )

  choice <- menu(c(
    chew_bold_green("Yes"),
    chew_bold_red("No!")
  ))

  clear_env <- function() {
    remove_env_var("CHEWIE_CACHE_HOME", renviron)
    return(invisible())
  }

  switch(choice,
    clear_env(),
    return(invisible())
  )
}

#' @title Get GEDI Cache
#' @rdname chewie-cache
#' @family manage cache
#' @export
chewie_get_cache <- function() {
  chewie_get_env("CHEWIE_CACHE_HOME")
}

#' @title Set GEDI Parquet Cache option params
#' @family manage cache
#' @noRd
chewie_set_cache_opts <- function() {
  options(
    chewie.parquet.cache = file.path(
      chewie_get_cache(),
      "GEDI-parquet-cache"
    ),
    chewie.h5.cache = file.path(
      chewie_get_cache(),
      "GEDI-h5-cache-temp"
    ),
    chewie.find.gedi.cache = file.path(
      chewie_get_cache(),
      "find-gedi-cache"
    )
  )
}
