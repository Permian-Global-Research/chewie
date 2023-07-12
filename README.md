<!-- README.md is generated from README.Rmd. Please edit that file -->

# chewie

<!-- badges: start -->

<!-- badges: end -->

The goal of chewie is to make downloading GEDI data much faster and simpler. Right now, in order to download a small area of GEDI, you are required to download very large files with irrelevant data. This package aims to overcome this issue by using the hdf driver from gdal to download only the data you need.

Then data should be saved as a geoarrow format and we should provide functions to enable efficient cleaning and filtering.

This package should not be a full suit of tools for working with GEDI data, but a simple and light way to access the data.

At present the rGEDI package is very challenging to install and requires that you download the very large files.

In general we don't need much of the functionality in rGEDI.

## Installation

You can install the development version of chewie like so:

``` r
# install.packages("pak")
pak::pkg_install("Permian-Global-Research/chewie")
```

\`\`\`