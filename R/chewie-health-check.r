#' @title Check the status of \{chewie\}
#' @description checks the NASA Earthdata Credentials and GEDI Cache
#' environment variables
#' @param .test logical; whether to test the credentials (requires internet
#' connection)
#' @param .report_cache logical; whether to report the cache status (default:
#' TRUE)
#'
#' @export
chewie_health_check <- function(.test = TRUE, .report_cache = TRUE) {
  assert_bool(.test)
  assert_bool(.report_cache)

  if (is.na(chewie_get_env())) {
    inform_env_health("No NASA Earthdata Credentials set.")
  } else {
    if (!file.exists(chewie_get_env())) {
      inform_env_health("NASA Earthdata Credentials file does not exist.")
    } else {
      cli::cli_inform(c(
        "v" =
          "NASA Earthdata Credentials already set."
      ), class = "packageStartupMessage")
    }
    if (isTRUE(.test)) {
      chewie_test_creds(.error = FALSE)
    }
  }

  if (is.na(chewie_get_cache())) {
    inform_cache_health("No GEDI Cache set.")
  } else {
    if (!dir.exists(chewie_get_cache())) {
      inform_cache_health("GEDI Cache directory does not exist.")
    } else {
      inform_cache_set_success(chewie_get_cache())

      gedi_find_cache_checker(.report_cache)

      gedi_grab_cache_checker(.report_cache)
    }
  }
}

#' @title get the size of a directory
#' @description Get the size of a directory including both n files and size in MB
#' @param dir character; the directory to check
#' @noRd
dir_info <- function(dir) {
  if (!dir.exists(dir)) {
    cli::cli_warn("Directory {dir} does not exist.")
  } else {
    # Get all files in the directory
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)

    # Get the sizes of the files
    file_sizes <- file.size(files)

    # Sum the file sizes to get the size of the directory
    return(list(
      nfiles = length(files),
      size = round(sum(file_sizes, na.rm = TRUE) / 1024 / 1024, 1)
    ))
  }
}

#' @title Check the GEDI find cache
#' @description Check the size of the GEDI find cache
#' @noRd
gedi_find_cache_checker <- function(.report) {
  if (!.report) {
    return(invisible())
  }

  if (dir.exists(getOption("chewie.find.gedi.cache"))) {
    gf_info <- dir_info(getOption("chewie.find.gedi.cache"))
    gf_fs <- gf_info$size
    gf_fn <- gf_info$nfiles
    inform_find_gedi_cache(gf_fn, gf_fs)
  }
}

#' @title Check the GEDI grab cache
#' @description Check the size of the GEDI grab cache
#' @noRd
gedi_grab_cache_checker <- function(.report) {
  if (!.report) {
    return(invisible())
  }

  if (dir.exists(getOption("chewie.parquet.cache"))) {
    parq_dirs <- list.files(
      getOption("chewie.parquet.cache"),
      full.names = TRUE
    )

    parq_info <- purrr::map(parq_dirs, dir_info) |>
      purrr::transpose() |>
      purrr::map(~ purrr::reduce(.x, c))

    cache_info <- tibble::tibble(
      `GEDI product` = basename(parq_dirs),
      `n files` = parq_info$nfiles,
      `cache size (MB)` = parq_info$size
    )
    class(cache_info) <- c("`grab_gedi` cache", "data.frame")
    attr(cache_info, "gedi_product") <- "Parquet store info:"
    chewie_print(cache_info, .printclass = FALSE, row.names = FALSE)
  }
}
