
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chewie

<img src="man/figures/chewie-hex.png"  align="right" height="300" style="float:right; height:300px;">

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of chewie is to make downloading GEDI data as fast and as
simple as possible. This includes point-level products: 1B, 2A, 2B and
4A. Here is a quick summary of design choices that enables {chewie} to
achieve this:

  - chewie adopts an R-centric approach to downloading GEDI data. Data
    are downloaded and converted to parquet files which can then be read
    using [{arrow}](https://arrow.apache.org/docs/r/index.html) and
    converted to [sf](https://r-spatial.github.io/sf/) objects. This
    approach is performative and enables the use of
    [dplyr](https://dplyr.tidyverse.org/) verbs to `filter`, `mutate`
    and `select` data as required without needing to load all shots,
    from a given swath/granule, into memory.

  - There is support for spatial filtering of granules that intersect an
    area of interest and not only by bounding box; this frequently
    reduces the amount of irrelevant data that is downloaded.

  - A system-level cache is used to store the data. This means that once
    a file has been downloaded it will not be downloaded again even if
    working in a different project (it is also possible to specify a
    different cache location for each project).

  - The scope of this package is deliberately narrow. It is not intended
    to include functionality for complex post processing or modelling.

## Installation

You can install the development version of chewie like so:

``` r
# install.packages("pak")
pak::pkg_install("Permian-Global-Research/chewie")
```

## Example

First, let’s load in some libraries. {dplyr} isn’t essential but it is
recommended as it’s an excellent and highly performative option for
working with arrow datasets.

``` r
library(chewie)
library(dplyr, warn.conflicts = FALSE)
library(sf)
```

Here are some useful helper functions to set up your credentials (using
`chewie_creds()`) and check that those credentials and the cache are set
up correctly (using `chewie_health_check()`). By default the cache is
set up in the `.chewie` folder in your home directory. You can change
this by running `chewie_cache_set()`.

``` r
chewie_creds() # to set up your credentials
chewie_health_check() # to check your credentials and cache setup.
```

In this chunk we search for some GEDI 2A data that intersects with the
Prairie Creek Redwoods State Park, California (the dataset is included
with the package). We then plot the footprints of the granules that
intersect with this area to check out what we’ve got. Note that by
default, both `find_gedi` and `grab_gedi` cache their outputs so when
these functions are re-run, the data will be loaded from the cache
rather than downloaded again, even in a different R session.

``` r
prairie_creek <- system.file("geojson", "nat-parks-hum.geojson", package = "chewie") |>
  sf::read_sf(
    query = "SELECT UNITNAME FROM \"nat-parks-hum\" WHERE UNITNAME = 'Prairie Creek Redwoods SP'"
  )

gedi_2a_search <- x <- find_gedi(prairie_creek,
  gedi_product = "2A",
  date_start = "2022-01-01",
  date_end = "2022-04-01"
)
#> ✔ Using cached GEDI find result

print(gedi_2a_search)
#> 
#> ── chewie.find ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • GEDI-2A
#>                     id          time_start            time_end                                                   url cached
#>                 <char>              <POSc>              <POSc>                                                <char> <lgcl>
#> 1: G2724056750-LPCLOUD 2022-01-22 01:09:09 2022-01-22 02:42:01 https://data.lpdaac.earthdatacloud.nasa.gov/lp-pro...   TRUE
#> 2: G2725123149-LPCLOUD 2022-03-01 09:59:35 2022-03-01 11:32:27 https://data.lpdaac.earthdatacloud.nasa.gov/lp-pro...   TRUE
#> 3: G2725124517-LPCLOUD 2022-03-05 08:24:36 2022-03-05 09:57:28 https://data.lpdaac.earthdatacloud.nasa.gov/lp-pro...   TRUE
#> 4: G2725130349-LPCLOUD 2022-03-10 12:13:49 2022-03-10 13:46:41 https://data.lpdaac.earthdatacloud.nasa.gov/lp-pro...   TRUE
#> 5: G2725131643-LPCLOUD 2022-03-14 10:39:08 2022-03-14 12:12:01 https://data.lpdaac.earthdatacloud.nasa.gov/lp-pro...   TRUE
#> 1 variable(s) not shown: [geometry <sfc_POLYGON>]
#> 
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
```

Whilst there is a `plot` method for *chewie.find* objects, a great
alternative is to plot a leaflet map with `chewie_show`, which can be
static or interactive (this uses the fantastic
{[mapview](https://r-spatial.github.io/mapview/)} under the hood).

``` r
chewie_show(
  gedi_2a_search,
  time_group = "month",
  zoom = 8
)
```

<img src="man/figures/README-show-find-data-1.png" width="100%" />

Now we use `grab_gedi` to download the data - this function internally,
converts the data to parquet format and stores it in the cache. The data
is as an arrow dataset. We can then use any {dplyr} verbs to
`filter`/`select` the data as we wish before finally using
`collect_gedi` to convert the data to a sf object. If no
filtering/selection is carried out then `collect_gedi` will return all
the available columns/rows.

``` r
gedi_2a_sf <- grab_gedi(gedi_2a_search) |>
  filter(
    quality_flag == 1,
    degrade_flag == 0
  ) |>
  select(
    beam, date_time, lat_lowestmode, lon_lowestmode, elev_highestreturn,
    elev_lowestmode, rh0, rh25, rh50, rh75, rh95, rh100
  ) |>
  collect_gedi(gedi_find = gedi_2a_search)
#> ✔ All data found in cache

print(gedi_2a_sf)
#> Simple feature collection with 1067 features and 10 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -124.0764 ymin: 41.34658 xmax: -123.9979 ymax: 41.41663
#> Geodetic CRS:  WGS 84
#> # A tibble: 1,067 × 11
#>     beam date_time           elev_highestreturn elev_lowestmode   rh0   rh25
#>  * <int> <dttm>                           <dbl>           <dbl> <dbl>  <dbl>
#>  1     1 2022-03-05 09:02:18               89.9            51.2 -1.61 17.1  
#>  2     1 2022-03-05 09:02:18              124.            112.  -4.08  0.590
#>  3     1 2022-03-05 09:02:19              138.            124.  -1.87  2.88 
#>  4     1 2022-03-05 09:02:19              126.             57.5 -2.28  5.99 
#>  5     1 2022-03-05 09:02:19              106.             90.9 -1.64  3.33 
#>  6     1 2022-03-05 09:02:19               86.9            24.2 -2.65  0.930
#>  7     1 2022-03-05 09:02:19              101.             24.9 -1.72 20.7  
#>  8     1 2022-03-05 09:02:19              137.            120.  -2.09  4    
#>  9     1 2022-03-05 09:02:19              182.            103.  -1.83 44.8  
#> 10     1 2022-03-05 09:02:19              197.            187.  -2.84  0.780
#> # ℹ 1,057 more rows
#> # ℹ 5 more variables: rh50 <dbl>, rh75 <dbl>, rh95 <dbl>, rh100 <dbl>,
#> #   geometry <POINT [°]>
```

Finally, we can plot the data. Again we can use the generic
`chewie_show` function.

``` r
chewie_show(
  gedi_2a_sf,
  zcol = "rh95",
  zoom = 12
)
```

<img src="man/figures/README-show-2a-data-1.png" width="100%" />

## Other relevant packages

  - [{rGEDI}](https://github.com/carlos-alberto-silva/rGEDI) provides
    the ability download GEDI data but also a great deal of additional
    functionality for visualisation, post-processing and modelling.

  - [{GEDI4R}](https://github.com/VangiElia/GEDI4R) which similarly
    provides a suit of tools for downloading, visualising and modelling
    GEDI data, but with a focus on the 4A product.

Both of these packages have been a great source of inspiration; we would
like to thank the authors for their great work\!
