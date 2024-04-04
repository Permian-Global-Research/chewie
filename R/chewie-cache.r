#' @title Manage the \{chewie\} cache
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
    renviron = c("auto", "user", "project"),
    quiet = FALSE) {
  renviron <- rlang::arg_match(renviron)
  assert_bool(quiet)
  dir_parquet <- file.path(.dir, "GEDI-parquet-cache")
  dir_h5 <- file.path(.dir, "GEDI-h5-cache-temp")
  di_find_gedi <- file.path(.dir, "find-gedi-cache")

  ovw <- add_env_var("CHEWIE_CACHE_HOME", .dir, renviron)

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
#'
#' @export
chewie_clear_h5_temp_cache <- function() {
  cache_dir <- getOption("chewie.h5.cache")

  clean_finds <- function() {
    list.files(cache_dir, pattern = "\\.h5$", full.names = TRUE) |>
      purrr::walk(file.remove)
  }

  choice <- cache_clear_check("H5")

  switch(choice,
    clean_finds(),
    return(invisible())
  )
}

#' @title Clear the GEDI parquet cache
#' @param directories character; the GEDI product parquet directories to clear.
#' @rdname chewie-cache
#' @family manage cache
#' @details
#' `chewie_clear_parquet_cache` deletes the cached .parquet files in the GEDI
#' parquet cache directory, located in `getOption("chewie.parquet.cache")`.
#'
#' This function should almost never be used. Situations where this may be
#' useful might include - requiring to free up disk space, if you are
#' experiencing issues with the cache, or if there are package updates which
#' render the existing cache irrelevant or incompatible.
#' @export
chewie_clear_parquet_cache <- function(
    directories = c("none", "1B", "2A", "2B", "4A")) {
  directories <- rlang::arg_match(directories, multiple = TRUE)

  clean_parquet <- function(pdir) {
    list.files(pdir, full.names = TRUE) |>
      purrr::walk(~ unlink(.x, recursive = TRUE))
    return(invisible())
  }

  if ("none" %in% directories) {
    inform_no_select_cache()
    return(invisible())
  }

  choice <- cache_clear_check(directories)
  p_caches <- file.path(getOption("chewie.parquet.cache"), directories)

  switch(choice,
    purrr::walk(p_caches, clean_parquet),
    return(invisible())
  )
}
