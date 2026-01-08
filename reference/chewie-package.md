# chewie: A package for downloading GEDI data.

The gedi package provides a set of functions for downloading and
carrying out simple pre processing of GEDI data. See below for a list of
these functions.

## Managing Credentials and Cache

- [`chewie_creds`](https://permian-global-research.github.io/chewie/reference/chewie-credentials.md):

  Set up and manage NASA Earthdata credentials

- [`chewie_health_check`](https://permian-global-research.github.io/chewie/reference/chewie_health_check.md):

  Quick diagnostics and health checks for the package cache and
  credentials.

- [`chewie_setup_cache`](https://permian-global-research.github.io/chewie/reference/chewie-cache.md):

  Set up and manage the package cache.

- [`chewie_unset_cache`](https://permian-global-research.github.io/chewie/reference/chewie-cache.md):

  Remove the `CHEWIE_PARQUET_CACHE` environment variable from the
  `.Renviron` file.

- [`chewie_get_cache`](https://permian-global-research.github.io/chewie/reference/chewie-cache.md):

  Get the path to the GEDI cache directory.

- [`chewie_clear_find_cache`](https://permian-global-research.github.io/chewie/reference/chewie-cache.md):

  Clear the find gedi cache.

- [`chewie_clear_h5_temp_cache`](https://permian-global-research.github.io/chewie/reference/chewie-cache.md):

  Clear the h5 temp cache.

- [`chewie_clear_parquet_cache`](https://permian-global-research.github.io/chewie/reference/chewie-cache.md):

  Clear the parquet gedi cache.

## Downloading GEDI Data

- [`find_gedi`](https://permian-global-research.github.io/chewie/reference/chewie-find-gedi.md):

  Find GEDI data in a given area for a given time period.

- [`grab_gedi`](https://permian-global-research.github.io/chewie/reference/grab_gedi.md):

  Download GEDI data and store in the cache.

## Reading GEDI Data

- [`collect_gedi`](https://permian-global-research.github.io/chewie/reference/collect_gedi.md):

  Collect GEDI data into a data frame from parquet files in the cache.

## Visualising GEDI Data

- [`chewie_show`](https://permian-global-research.github.io/chewie/reference/chewie_show.md):

  Visualise GEDI data on a map.

## See also

Useful links:

- <https://github.com/Permian-Global-Research/chewie>

- <https://permian-global-research.github.io/chewie/>

- Report bugs at
  <https://github.com/Permian-Global-Research/chewie/issues>

## Author

**Maintainer**: Hugh Graham <hugh.graham@permianglobal.com>
([ORCID](https://orcid.org/0000-0001-9451-5010))
