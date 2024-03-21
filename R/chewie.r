#' @title chewie: A package for downloading GEDI data.
#'
#' @description The gedi package provides a set of functions for downloading and carrying out
#' simple pre processing of GEDI data. See below for a list of these functions.
#' @keywords internal
#' @section Managing Credentials and Cache:
#' \describe{
#'   \item{\code{\link{chewie_creds}}}{Set up and manage NASA Earthdata credentials}
#'   \item{\code{\link{chewie_health_check}}}{Quick diagnostics and health checks for the package cache and credentials.}
#'   \item{\code{\link{chewie_setup_cache}}}{Set up and manage the package cache.}
#'   \item{\code{\link{chewie_unset_cache}}}{Remove the `CHEWIE_PARQUET_CACHE` environment variable from the `.Renviron` file.}
#'   \item{\code{\link{chewie_get_cache}}}{Get the path to the GEDI cache directory.}
#'   \item{\code{\link{chewie_clear_find_cache}}}{Clear the find gedi cache.}
#'   \item{\code{\link{chewie_clear_h5_temp_cache}}}{Clear the h5 temp cache.}
#'   \item{\code{\link{chewie_clear_parquet_cache}}}{Clear the parquet gedi cache.}
#' }
#' @section Downloading GEDI Data:
#' \describe{
#'  \item{\code{\link{find_gedi}}}{Find GEDI data in a given area for a given time period.}
#' \item{\code{\link{grab_gedi}}}{Download GEDI data and store in the cache.}
#' }
#'
#' @section Reading GEDI Data:
#' \describe{
#' \item{\code{\link{collect_gedi}}}{Collect GEDI data into a data frame from parquet files in the cache.}
#' }
#'
#' @section Visualising GEDI Data:
#' \describe{
#' \item{\code{\link{chewie_show}}}{Visualise GEDI data on a map.}
#' }

"_PACKAGE"
