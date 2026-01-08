# Collect GEDI data into an sf object

Collect GEDI data, returned from `grab_gedi`, as an sf object.

## Usage

``` r
collect_gedi(
  x,
  gedi_find,
  intersects = attributes(gedi_find)$intersects,
  drop_xy_vars = TRUE
)
```

## Arguments

- x:

  An arrow dataset object.

- gedi_find:

  The chewie.find object used to obtain `x`.

- intersects:

  logical; whether to filter the GEDI data based on the search extent
  attributed to the `chewie.find` object. Default is to use whatever was
  specified in chewie.find.

- drop_xy_vars:

  logical; whether to drop the columns used to create the geometry
  column. Default is `TRUE`.

## Value

an sf object

## Details

This function is used to collect the GEDI data returned from `grab_gedi`
as an sf object. It is largely a wrapper for dplyr::collect but converts
to sf and filters the gedi footprints based on the search extent
attributed to the `chewie.find` object. It is strongly recomended that
you make the most of the ability to to edit the gedi data on read by
using the `dplyr` verbs before collecting the data. This will save a lot
of time and memory. However, make sure that, when selecting columns, you
do not remove the `lat_lowestmode` and `lon_lowestmode` columns as these
are required to create the geometry column.

## Examples

``` r
if (FALSE) { # interactive()
prairie_creek <- sf::read_sf(
  system.file("geojson", "prairie-creek.geojson", package = "chewie")
)
prairie_creek_find_4a <- find_gedi(prairie_creek,
  gedi_product = "4A",
  date_start = "2022-01-01", date_end = "2022-04-01",
  cache = FALSE
)

prairie_creek_grab_4a <- grab_gedi(prairie_creek_find_4a)

prairie_creek_4a_sf <- collect_gedi(
  prairie_creek_grab_4a,
  prairie_creek_find_4a
)
print(prairie_creek_4a_sf)
}
```
