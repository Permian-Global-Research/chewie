# --- Define the testing area.
pcreek <- system.file(
  "geojson", "prairie-creek.geojson",
  package = "chewie"
) |>
  sf::read_sf()

# --- Setup the testing environment.
windows_tz_dir <- chewie:::download_tzdata_on_windows_gha()
aum <- options(arrow.unsafe_metadata = TRUE)
temp_dir <- tempdir()
old_chewie_cache <- chewie::chewie_get_cache()
options("chewie.testing" = TRUE)

file.create(".Renviron")

cache_zip <- system.file(
  "chewie-test-cache.zip",
  package = "chewie"
)

unzipped_files <- utils::unzip(
  cache_zip,
  exdir = temp_dir
)

chewie::chewie_setup_cache(file.path(temp_dir, "chewie-test-cache"),
  renviron = "project"
)
# --- teardown section - tidying up
withr::defer(
  {
    chewie::chewie_setup_cache(old_chewie_cache)
    fs::file_delete(unzipped_files)
    options(chewie.testing = NULL)
    options(aum)
    fs::file_delete(".Renviron")
    if (!is.null(windows_tz_dir)) {
      fs::dir_delete(windows_tz_dir)
    }
  },
  envir = teardown_env()
)
