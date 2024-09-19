#' Download the latest IANA timezone database on Windows
#' A helper function taken from https://github.com/apache/arrow/blob/a6f736c962adddd0f7078a10338fe8f1445c583b/python/pyarrow/util.py#L233
#' @return Invisible
#' @details For some reason we have failures on windows relating to the timezone
#' database. This function is a workaround to download the latest IANA timezone
#' database on Windows. It is used in tests and vignettes.
#' @noRd
#' @keywords internal
download_tzdata_on_windows <- function(tzdir = NULL) {
  if (.Platform$OS.type != "windows") {
    cli::cli_alert_info(
      "Timezone database is already provided by {(.Platform)$OS.type}"
    )
    return(NULL)
  }

  # Define paths
  if (is.null(tzdir)) {
    tzdata_path <- path.expand(
      file.path(Sys.getenv("USERPROFILE"), "Downloads", "tzdata")
    )
  } else {
    tzdata_path <- tzdir
  }
  tzdata_compressed <- file.path(tzdata_path, "tzdata.tar.gz")

  # Create directory if it doesn't exist
  if (!dir.exists(tzdata_path)) {
    dir.create(tzdata_path, recursive = TRUE)
  }

  # Download the latest IANA timezone database
  utils::download.file(
    "https://data.iana.org/time-zones/tzdata-latest.tar.gz", tzdata_compressed,
    mode = "wb"
  )

  # Check if the file exists
  if (!file.exists(tzdata_compressed)) {
    stop("Failed to download the timezone database.")
  }

  # Extract the tar.gz file
  utils::untar(tzdata_compressed, exdir = tzdata_path)

  # Download the windowsZones.xml file
  windows_zones_url <- paste0(
    "https://raw.githubusercontent.com/unicode-org/",
    "cldr/master/common/supplemental/windowsZones.xml"
  )
  windows_zones_path <- file.path(tzdata_path, "windowsZones.xml")
  utils::download.file(windows_zones_url, windows_zones_path, mode = "wb")
  return(tzdata_path)
}

#' wrapper for download_tzdata_on_windows for GitHub Actions
#' @noRd
#' @keywords internal
download_tzdata_on_windows_gha <- function() {
  if (Sys.getenv("RUNNER_OS") == "Windows") {
    return(download_tzdata_on_windows("C:/Users/runneradmin/Downloads/tzdata"))
  } else {
    cli::cli_alert_info(
      "Not running on Windows GitHub Action"
    )
    return(NULL)
  }
}
