# Find GEDI granules

Find GEDI granules that intersect with a given spatial object.

## Usage

``` r
find_gedi(
  x,
  gedi_product = c("1B", "2A", "2B", "4A"),
  date_start = NULL,
  date_end = NULL,
  intersects = TRUE,
  cache = TRUE
)
```

## Arguments

- x:

  object of class `sf`, `spatVector`, `spatRaster`, `sfc`, `stars`,
  `stars_proxy`, or `numeric` see details. Used to define the search
  area for GEDI data.

- gedi_product:

  character of GEDI product to search for.

- date_start:

  character or `POSIXct` of the start date to search for GEDI data. If
  `NULL` defaults to the start of GEDI operations (2019-03-25).

- date_end:

  character or `POSIXct` of the end date to search for GEDI data. If
  `NULL` defaults to the current date.

- intersects:

  logical indicating whether to return only granules that intersect with
  the given spatial object. If FALSE all granules that are contained
  within the bounding box of the spatial object are returned.

- cache:

  logical indicating whether to cache the results of the GEDI search. If
  TRUE the results of the search will be cached in the directory defined
  by: `getOption("chewie.find.gedi.cache")`. If FALSE the results will
  not be cached.

## Details

Where x is a numeric it must be of length 4 with values corresponding to
the bounding box coordinates in the order xmin, ymin, xmax, ymax.

By default, the {sf} package uses the s2 model to carry out geometric
operations. This can sometimes result in the apparent intersection of
GEDI granules and an AOI, possibly resulting in the downloading of more
data than expected. If this is an issue in your case, you can use
`sf::sf_use_s2(use_s2 = FALSE)`. See
[sf_use_s2](https://r-spatial.github.io/sf/reference/s2.html) for more
details.

## Examples

``` r
if (FALSE) { # interactive()

humboldt <- sf::read_sf(
  system.file("geojson", "humboldt.geojson", package = "chewie")
)
humboldt_find_2a <- find_gedi(humboldt,
  gedi_product = "2A",
  date_start = "2022-01-01",
  date_end = "2022-04-01",
  cache = FALSE
)
print(humboldt_find_2a)
}
```
