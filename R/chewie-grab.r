#' @title Download GEDI data or access from cahce
#' @description Download GEDI data from the NASA Earthdata in hdf5 format.
#' @param x A chewie.find.x object.
#' dataset. See details.
#' @param progress A logical indicating whether to show a progress bar.
#' @param timeout A numeric indicating the timeout in seconds.
#' @param batchsize A numeric indicating the number of files to download in
#' parallel. where batchsize is less than the number of files to download, the
#' files will be downloaded in chunks of batchsize.
#' @param delete_h5 A logical indicating whether to delete the hdf5 file after
#' conversion to parquet. Default is TRUE. these files are saved in
#' `getOption("chewie.h5.cache")`.
#' @param compression A character vector indicating the compression codec to
#' use. Default is `getOption("chewie.parquet.codec")`. see
#' `?arrow::write_parquet`. must be one of: "zstd", "brotli", "gzip", "snappy",
#' "bz2", "lz4", "lzo" or "uncompressed".
#' @export
#' @returns An arrow_dplyr_query object.
#' @details
#' This function is the main handler for gedi data - it checks the cache to see
#' if the required GEDI data are already downloaded, and if not, downloads them
#' from the NASA Earthdata cloud. Once downloaded each file is converted to
#' parquet format and saved in the cache directory. This saves a huge amount of
#' disk space and enables dynamic reading and filtering of the returned "open"
#' arrow dataset.
#'
#'
#' \{chewie\} will only cache specific variables made available in the GEDI hdf5
#' files. This is in part to reduce disk space but also to improve performance
#' and make working with these data simpler. If you require additional variables
#' to be cached, please raise an issue on the \{chewie\} GitHub repository.
#'
#' @seealso
#' For more information on the GEDI hdf5 files and the variables they contain
#' see the following links:
#'
#' 1B: https://lpdaac.usgs.gov/documents/585/gedi_l1b_product_data_dictionary_P003_v1.html
#'
#' 2A: https://lpdaac.usgs.gov/documents/982/gedi_l2a_dictionary_P003_v2.html
#'
#' 2B: https://lpdaac.usgs.gov/documents/587/gedi_l2b_dictionary_P001_v1.html
#'
#' 4A: https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density_V2_1.html
#'
#'
#' @examplesIf interactive()
#' prairie_creek <- sf::read_sf(
#'   system.file("geojson", "prairie-creek.geojson", package = "chewie")
#' )
#' prairie_creek_find_2b <- find_gedi(prairie_creek,
#'   gedi_product = "2B",
#'   date_start = "2022-01-01", date_end = "2022-04-01",
#'   cache = FALSE
#' )
#'
#' prairie_creek_grab_2b <- grab_gedi(
#'   prairie_creek_find_2b
#' )
#'
grab_gedi <- function(
    x,
    progress = TRUE, timeout = 7200,
    batchsize = 10, delete_h5 = TRUE,
    compression = getOption("chewie.parquet.codec")) {
  .dir <- getOption("chewie.h5.cache")
  assert_classes(x, "chewie.find")
  assert_bool(progress)
  assert_numeric(timeout)
  assert_numeric(batchsize)
  assert_bool(delete_h5)

  compression <- rlang::arg_match(
    compression,
    c("zstd", "brotli", "gzip", "snappy", "bz2", "lz4", "lzo", "uncompressed")
  )
  st_time <- Sys.time()

  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive = TRUE)
  }

  gedi_product <- find_gedi_product(x)

  # function to control file download and conversion.
  x_to_down <- chewie_missing_gedi(x) |>
    dplyr::mutate(destfile = file.path(.dir, basename(url)))

  # get the number of files to download for informative messaging later.
  nfiles <- nrow(x_to_down)

  if (nfiles > 0) {
    # split the urls into chunks to download in parallel
    x2d_chunks <- x_to_down |>
      dplyr::mutate(group = ceiling(
        seq_len(dplyr::n()) /
          (dplyr::n() /
            (dplyr::n() / batchsize))
      )) |>
      dplyr::group_by(group) |>
      dplyr::group_split()

    # TODO: is this needed?
    # urlchunks <- setNames(x2d_chunks, paste0("chunk_", seq_along(x2d_chunks)))

    dl_df <- download_wrap(
      x2d_chunks, .dir, timeout,
      progress, gedi_product,
      delete_h5, compression
    )

    log_staus_codes(dl_df, nfiles)
    inform_time(st_time, "Download")
  }

  return(open_gedi(x))
}

#' @title convert GEDI hdf5 to parquet
#' @description internal function to convert GEDI hdf5 files to parquet format.
#' @param df a dataframe returned from `curl::multi_download`.
#' @param .dir A character vector of the directory to save the parquet file.
#' @param timeout A numeric indicating the timeout in seconds.
#' @param progress A logical indicating whether to show a progress bar.
#' @param gedi_prod A character vector indicating the GEDI product.
#' @param nfiles A numeric indicating the number of files to download.
#' dataset.
#' @param delete_h5 A logical indicating whether to delete the hdf5 file after
#' conversion to parquet.
#' @param codec A character vector indicating the compression codec to use.
#' @noRd
chewie_mk_parquet <- function(
    df,
    .dir, timeout, progress, gedi_prod, nfiles, delete_h5, codec) {
  s_id <- df$id

  attr(df, "class") <- c(
    paste0("chewie.download.", gedi_prod),
    class(df)
  )

  if (isTRUE(check_status_codes(df))) {
    gedi_dt <- chewie_convert(df)
    save_dir <- file.path(
      getOption(
        "chewie.parquet.cache"
      ),
      gedi_prod,
      paste0("granule_id=", s_id)
    )
    check_n_make_dir(save_dir)

    arrow::write_parquet(
      dplyr::as_tibble(gedi_dt),
      file.path(
        save_dir,
        paste0(
          tools::file_path_sans_ext(basename(df$destfile)),
          ".parquet"
        )
      ),
      compression = codec # TODO: this needs some thought...
    )

    if (isTRUE(delete_h5)) {
      file.remove(df$destfile)
    }
  }

  gc()

  return(df)
}

#' @title filter find for uncached data
#' @description internal function to filter a chewie.find object for data that
#' is not already cached.
#' @param x A chewie.find object.
#' @details mainly for more informative messaging.
#' @noRd
chewie_missing_gedi <- function(x) {
  x <- chewie_scan(x)
  n_swaths <- nrow(x)
  to_download <- x[!x$cached, ]

  if (nrow(to_download) == 0) {
    cli::cli_alert_success("All data found in cache")
  } else {
    if (nrow(to_download) != n_swaths) {
      cli::cli_inform(
        c(
          "i" = "{n_swaths - nrow(to_download)} of {n_swaths}
                    file{?s}  found in cache"
        )
      )
    }
  }
  return(to_download)
}

check_status_codes <- function(x) {
  # TODO: what do we need here -  are we okay with just the 2nd condition?
  if (all(x$status_code %in% c(200, 206, 416)) || all(x$success)) {
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

    abort_download_with_log(log_path)
  }

  completed <- which(x$status_code == 416)
  if (length(completed) > 0) {
    inform_download_completed(length(completed), n)
  }
  return(invisible())
}


#' Download GEDI data from the LPDAAC
#' @param url_batched A list of character vectors of urls to download.
#' @param dir A character vector of the directory to save the hdf5 file.
#' @param timeout A numeric indicating the timeout in seconds.
#' @param progress A logical indicating whether to show a progress bar.
#' @param gedi_product A character vector indicating the GEDI product.
#' dataset.
#' @param delete_h5 A logical indicating whether to delete the hdf5 file after
#' conversion to parquet.
#' @param codec A character vector indicating the compression codec to use.
#' @noRd
#' @keywords internal
download_wrap <- function(
    url_batched, dir, timeout,
    progress, gedi_product, delete_h5, codec) {
  n <- sum(vapply(url_batched, nrow, 1L))
  n_batch <- length(url_batched)
  if (n_batch == 1) {
    n_batch <- NULL
  }
  inform_n_to_download(gedi_product, n, n_batch)

  dl_f <- function(x2d_chunk) {
    df_down <- curl::multi_download(
      x2d_chunk$url,
      destfiles = normalizePath(
        file.path(dir, basename(x2d_chunk$url)),
        mustWork = FALSE
      ),
      resume = TRUE,
      timeout = timeout,
      progress = progress,
      multiplex = TRUE,
      netrc = TRUE,
      netrc_file = Sys.getenv("CHEWIE_NETRC")
    ) |>
      dplyr::mutate(destfile = normalizePath(destfile, mustWork = FALSE))


    dd_full_split <- sf::st_drop_geometry(x2d_chunk) |>
      dplyr::mutate(destfile = normalizePath(destfile, mustWork = FALSE)) |>
      dplyr::select(destfile, id) |>
      # dplyr::mutate(row_num = dplyr::row_number()) |>
      dplyr::right_join(df_down, by = "destfile") |>
      dplyr::group_split(dplyr::row_number())


    inform_n_to_convert(gedi_product, length(dd_full_split))

    purrr::map( #
      dd_full_split,
      ~ chewie_mk_parquet(
        .x,
        dir, timeout, progress, gedi_product,
        nrow(x2d_chunk), delete_h5, codec
      ),
      .progress = progress
    ) |>
      purrr::list_rbind()
  }

  # here we run the download chunks in parallel
  options(cli.spinner = "earth")
  purrr::map(
    url_batched,
    dl_f,
    .progress = list(
      type = "iterator",
      format =
        "{cli::pb_spin} Downloading chunk {cli::pb_current}/{cli::pb_total}",
      clear = TRUE
    )
  ) |>
    purrr::list_rbind()
}
