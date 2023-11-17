
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chewie<img src="man/figures/chewie-hex.png" align="right" height="200" alt=""/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of chewie is to make downloading GEDI data as fast and as
simple as possible. This includes point-level products: 1B, 2A, 2B and
4A. Here is a quick summary of design choices that enables {chewie} to
achieve this:

  - chewie adopts an R-centric approach to downloading GEDI data. Data
    are downloaded and converted to parquet files which can then read
    using the [{arrow}](https://arrow.apache.org/docs/r/index.html) and
    converted to [sf](https://r-spatial.github.io/sf/) objects. This
    approach is performative and enables the use of
    [dplyr](https://dplyr.tidyverse.org/) verbs to `filter`, `mutate`
    and `select` data as required without needing to load all shots,
    from a given swath, into memory.

  - There is support for spatial filtering of swaths that intersect an
    area of interest and not only by bounding box; this frequently
    reduces the amount of irrelevant data that is downloaded.

  - A system-level cache is used to store the data. This means that once
    a file has been downloaded it will not be downloaded again even if
    working in a different project (it is also possible to specify a
    different cache location for each project).

  - The scope of this package is deilibertly narrow. It is not intended
    to include functionality for post processing or modelling.

TO DO:

  - [ ] Add a `chewie_show` method to plot footprints

  - [ ] Add a functionality to get level 4a data

  - [ ] Add cache reporting to `chewie_health_check` - i.e. n files in
    cache, size of cache, etc.

  - [ ] write tests…

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
#> ✔ NASA Earthdata Credentials already set.
#> ✔ GEDI cache set in the following directory:
#> → "/home/hugh/.chewie"
library(dplyr, warn.conflicts = FALSE)
library(sf)
#> Linking to GEOS 3.11.1, GDAL 3.6.4, PROJ 9.1.1; sf_use_s2() is TRUE
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
Haywood county in North Carolina. We then plot the footprints of the
swaths that intersect with this area to check out what we’ve got. Note
that by default, both `find_gedi` and `grab_gedi` cache their outputs so
when these functions are re-run, the data will be loaded from the cache
rather than downloaded again, even in a different R session.

we can print and plot the results of `find_gedi` to check that we have
the data we want.

``` r
nc <- system.file("gpkg", "nc.gpkg", package = "sf")
hw <- subset(read_sf(nc), NAME == "Haywood")

gedi_2a_search <- find_gedi(hw,
  gedi_product = "2A",
  date_start = "2022-12-31"
)
#> ! No end date was provided - A Non-permenant cache is in effect.
#> ℹ The cache will be invalidated on 2024-01-01T00:00:00.
#>   To establish a permanent cache set the end date using the `date_end`
#>   argument.
#> ✔ Using cached GEDI data

print(gedi_2a_search)
#> 
#> ── chewie.find ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> •  GEDI-2A
#>                        id          time_start            time_end                                                   url cached
#>                    <char>              <POSc>              <POSc>                                                <char> <lgcl>
#> 1: G2752113657-LPDAAC_ECS 2023-01-09 02:25:28 2023-01-09 03:58:19 https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI0...   TRUE
#> 2: G2752559659-LPDAAC_ECS 2023-01-11 08:34:47 2023-01-11 10:07:37 https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI0...   TRUE
#> 3: G2752391704-LPDAAC_ECS 2023-01-31 17:30:11 2023-01-31 19:03:00 https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI0...   TRUE
#> 4: G2752261157-LPDAAC_ECS 2023-02-02 23:38:56 2023-02-03 01:11:45 https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI0...   TRUE
#> 5: G2753021606-LPDAAC_ECS 2023-02-04 15:53:09 2023-02-04 17:26:01 https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI0...   TRUE
#> 1 variable not shown: [geometry <sfc_POLYGON>]
#> 
#> ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

plot(gedi_2a_search)
```

<img src="man/figures/README-find-data-1.png" width="100%" />

Or alternatively plot a leaflet map with `chewie_show`, which can be
static or interactive.

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
is as an arrow dataset. We can then use any dplyr verbs to filter/select
the data as we wish before finally using `collect_gedi` to convert the
data to a sf object.

``` r
gedi_2a_sf <- grab_gedi(gedi_2a_search) |>
  filter(
    quality_flag == 1,
    degrade_flag == 0
  ) |>
  select(
    beam, date_time, solar_elevation, lat_lowestmode, lon_lowestmode,
    elev_highestreturn, elev_lowestmode, rh0, rh25, rh50, rh75, rh100
  ) |>
  collect_gedi(gedi_find = gedi_2a_search)
#> ✔ All data found in cache

print(gedi_2a_sf)
#> Simple feature collection with 2798 features and 12 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -82.95697 ymin: 35.29232 xmax: -82.74454 ymax: 35.44537
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,798 × 13
#>    beam  date_time           solar_elevation lat_lowestmode lon_lowestmode
#>  * <chr> <dttm>                        <dbl>          <dbl>          <dbl>
#>  1 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  2 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  3 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  4 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  5 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  6 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  7 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  8 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#>  9 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#> 10 0     2023-02-04 16:28:13            35.3           35.3          -83.0
#> # ℹ 2,788 more rows
#> # ℹ 8 more variables: elev_highestreturn <dbl>, elev_lowestmode <dbl>,
#> #   rh0 <dbl>, rh25 <dbl>, rh50 <dbl>, rh75 <dbl>, rh100 <dbl>,
#> #   geometry <POINT [°]>

plot(gedi_2a_sf["rh75"], axes = TRUE, reset = FALSE)
plot(sf::st_transform(hw[0], sf::st_crs(gedi_2a_sf)), add = TRUE, reset = FALSE)
```

<img src="man/figures/README-collect-data-1.png" width="100%" />

## Other relevant packages

  - [{rGEDI}](https://github.com/carlos-alberto-silva/rGEDI) provides
    the ability download GEDI data but also a great deal of additional
    functionality for visualisation, post processing and modelling.

  - [{GEDI4R}](https://github.com/VangiElia/GEDI4R) which similiarly
    provides a suit of tools for downloading, visualising and modelling
    GEDI data, but with a focus on the 4A product.

Both of these packages have been a great source of inspiration for this
package we would like to thank the authors for their great work\!
