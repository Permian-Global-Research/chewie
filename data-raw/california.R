## code to prepare `california` dataset goes here
#' @title get administritive outlines for a country
#' @description using the geoBoundaires API,
#' download the geojson outline of a country
#' @param country character vector of country names
#' @param admin_level character vector of admin levels to download
#' @return sf object of the outlines
#' @details check out the documentation for the geoboundaries API at:
#' geoBoundaries.org
#'
geo_bounds <- function(country, admin_level = c("ADM0", "ADM1", "ADM2")) {
  country <- countrycode::countrycode(country,
    origin = "country.name",
    destination = "iso3c"
  )
  url <- paste(
    "https://www.geoboundaries.org/api/current/gbOpen/",
    country, admin_level[1],
    sep = "/"
  )
  get <- httr::GET(url)
  cont <- httr::content(get, as = "parsed")
  area <- sf::read_sf(cont$gjDownloadURL)
  return(area)
}
california <- geo_bounds("usa", "ADM1") |>
  sf::st_transform(4326) |>
  dplyr::filter(shapeName == "California")

humboldt <- geo_bounds("usa", "ADM2") |>
  sf::st_transform(4326) |>
  dplyr::filter(shapeID == "52423323B26488914054819")


if (!dir.exists("inst/geojson")) {
  dir.create("inst/geojson")
}

sf::write_sf(california, "inst/geojson/california.geojson", delete_dsn = TRUE)
sf::write_sf(humboldt, "inst/geojson/humboldt.geojson", delete_dsn = TRUE)
