#' @title Set GEDI Parquet Cache
#' @param .dir character path to the directory that should contain the cache.
#' @param renviron character either 'user', '"project"' or path to the directory
#' containing the `.Renviron` file to set the `CHEWIE_PARQUET_CACHE`
#' environment and create the directory. If missing, the default cache
#' directory is used.
#' @param quiet logical if `TRUE` then no messages are printed.
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
#' @description Get the path to the GEDI cache directory.
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

#' @title Clear the GEDI find (search) cache
#' @rdname chewie-cache
#' @family manage cache
#' @details
#' `chewie_clear_find_cache` deletes the cached .rds files in the GEDI find
#' cache directory, located in `getOption("chewie.find.gedi.cache")`.
#' @export
chewie_clear_find_cache <- function() {
  cache_dir <- getOption("chewie.find.gedi.cache")

  clean_finds <- function() {
    list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE) |>
      purrr::walk(file.remove)
  }

  cli::cli_inform(
    paste0(
      chew_bold_mag("?"),
      paste0(
        "   Do you really want to clear your GEDI search cache?"
      )
    )
  )

  choice <- menu(c(
    chew_bold_green("Yes"),
    chew_bold_red("No!")
  ))

  switch(choice,
    clean_finds(),
    return(invisible())
  )
}

#' @title Clear the GEDI h5 temp cache
#' @rdname chewie-cache
#' @family manage cache
#' @details
#' `chewie_clear_h5_temp_cache` deletes the cached .h5 files in the GEDI h5 -
#' these files sometimes persist when a download in incomplete or there has
#' been an error with the download. If you are having trouble downloading data,
#' running this command could well help.
#'
#' chewie does not provide a helper function to clear the main parquet cache
#' for safety reasons. In theory this cache should also be stable. To manually
#' clear it - navigate to the location of `getOption("chewie.parquet.cache")`
#' and delete the necessary files.
#'
#' @export
chewie_clear_h5_temp_cache <- function() {
  cache_dir <- getOption("chewie.h5.cache")

  clean_finds <- function() {
    list.files(cache_dir, pattern = "\\.h5$", full.names = TRUE) |>
      purrr::walk(file.remove)
  }

  cli::cli_inform(
    paste0(
      chew_bold_mag("?"),
      paste0(
        "   Do you really want to clear your GEDI h5 temp cache?"
      )
    )
  )

  choice <- menu(c(
    chew_bold_green("Yes"),
    chew_bold_red("No!")
  ))

  switch(choice,
    clean_finds(),
    return(invisible())
  )
}
