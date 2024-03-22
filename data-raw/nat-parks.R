library(sf)

# download park boundaries from here:
# https://www.parks.ca.gov/?page_id=29682
np_url <- paste0(
  "https://opendata.arcgis.com/datasets/0fea4fa1db734794bdb3b5410bb3eed9_0.kml",
  "?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
)
np_dest <- tempfile(fileext = ".kml")
download.file(np_url, np_dest)

np_boundaries <- sf::st_read(np_dest) |>
  sf::st_make_valid()
mapview(np_boundaries, alpha.regions = 0.1)

hum <- read_sf(system.file("geojson", "humboldt.geojson", package = "chewie"))

np_hum <- st_filter(np_boundaries, hum)


write_sf(np_hum, "inst/geojson/nat-parks-hum.geojson", delete_dsn = TRUE)

system.file(
  "geojson", "nat-parks-hum.geojson",
  package = "chewie"
) |>
  sf::read_sf(
    query = "SELECT UNITNAME FROM \"nat-parks-hum\" WHERE UNITNAME = 'Prairie Creek Redwoods SP'"
  ) |>
  write_sf("inst/geojson/prairie-creek.geojson", delete_dsn = TRUE)
