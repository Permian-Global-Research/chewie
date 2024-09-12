# --- Setup the testing environment.
options("chewie.testing" = TRUE)
temp_dir <- tempdir()
setwd(temp_dir)
if (!file.exists(".Renviron")) {
  file.create(".Renviron")
}

cache_zip <- system.file(
  "chewie-test-cache.zip",
  package = "chewie"
)

utils::unzip(
  cache_zip,
  exdir = temp_dir
)
temp_cache <- file.path(temp_dir, "chewie-test-cache")

chewie::chewie_setup_cache(temp_cache, renviron = "project")

# --- Define the testing area.
pcreek <- system.file(
  "geojson", "prairie-creek.geojson",
  package = "chewie"
) |>
  sf::read_sf()
