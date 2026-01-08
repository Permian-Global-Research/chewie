# Show an interactive map of a chewie\_\* object.

Show an interactive map of a chewie\_\* object.

## Usage

``` r
chewie_show(x, ...)

# S3 method for class 'sf'
chewie_show(
  x,
  zcol = NULL,
  alpha = 0.8,
  border_alpha = 0.01,
  pal = "Plasma",
  pal_reverse = FALSE,
  aoi_color = "black",
  zoom = NULL,
  interactive = TRUE,
  file = tempfile(fileext = ".png"),
  ...
)

# S3 method for class 'chewie.find'
chewie_show(
  x,
  time_group = c("year", "month"),
  alpha = 0.5,
  border_alpha = 0.5,
  pal = "Zissou 1",
  pal_reverse = FALSE,
  aoi_color = "black",
  zoom = NULL,
  interactive = TRUE,
  file = tempfile(fileext = ".png"),
  ...
)
```

## Arguments

- x:

  object of class `chewie_find` or `chewie_grab`.

- ...:

  additional arguments passed to
  [mapview](https://r-spatial.github.io/mapview/reference/mapView.html).

- zcol:

  character; name of the column to use for coloring footprints.

- alpha:

  numeric; transparency of the granules or footprints.

- border_alpha:

  numeric; transparency of the granule/footprint borders.

- pal:

  character; name of the color palette to use for the
  granules/footprints. see
  [`hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html) for a list
  of available palettes.

- pal_reverse:

  logical; if `TRUE`, reverse the color palette.

- aoi_color:

  character; color of the AOI polygon.

- zoom:

  numeric; zoom level of the map. If `NULL` (default), the zoom level is
  determined automatically by the extent of the GEDI granules.

- interactive:

  logical; if `TRUE` (default), return an interactive map. If `FALSE`,
  return a static map.

- file:

  character; if `interactive = FALSE`, save the static map to this file.

- time_group:

  character; if `interactive = TRUE`, group the granules by this time
  unit. Must be one of `"year"` or `"month"`.

## Value

interactive leaflet map if `interactive = TRUE`, otherwise the path to
the saved static map image.

## Details

This function is a wrapper around
[mapview](https://r-spatial.github.io/mapview/reference/mapView.html).
It is designed to work with chewie.find objects generated with
[find_gedi](https://permian-global-research.github.io/chewie/reference/chewie-find-gedi.md)
and sf objects returned by
[collect_gedi](https://permian-global-research.github.io/chewie/reference/collect_gedi.md).

## Examples

``` r
if (FALSE) { # interactive()
prairie_creek <- sf::read_sf(system.file(
  "geojson", "prairie-creek.geojson",
  package = "chewie"
))

gedi_2a_search <- x <- find_gedi(prairie_creek,
  gedi_product = "2A",
  date_start = "2022-01-01",
  date_end = "2022-04-01"
)

chewie_show(gedi_2a_search, zoom = 8)
chewie_show(
  gedi_2a_search,
  zoom = 10,
  interactive = FALSE,
  file = tempfile(fileext = ".png")
)

gedi_2a_sf <- grab_gedi(gedi_2a_search) |>
  collect_gedi(gedi_find = gedi_2a_search)

chewie_show(
  gedi_2a_sf,
  zcol = "rh95",
  zoom = 13,
  alpha = 0.5,
)
}
```
