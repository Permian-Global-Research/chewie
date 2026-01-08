# Manage the {chewie} cache

Get the path to the GEDI cache directory.

## Usage

``` r
chewie_setup_cache(
  .dir = chewie_default_dir(),
  renviron = c("auto", "user", "project"),
  quiet = FALSE
)

chewie_unset_cache(renviron = "user")

chewie_get_cache()

chewie_clear_find_cache()

chewie_clear_h5_temp_cache()

chewie_clear_parquet_cache(directories = c("none", "1B", "2A", "2B", "4A"))
```

## Arguments

- .dir:

  character path to the directory that should contain the cache.

- renviron:

  character either 'user', '"project"' or path to the directory
  containing the `.Renviron` file to set the `CHEWIE_PARQUET_CACHE`
  environment and create the directory. If missing, the default cache
  directory is used.

- quiet:

  logical if `TRUE` then no messages are printed.

- directories:

  character; the GEDI product parquet directories to clear.

## Details

This `chewie_unset_cache` function will remove the
`CHEWIE_PARQUET_CACHE` environment variable from the `.Renviron` file.

`chewie_clear_find_cache` deletes the cached .rds files in the GEDI find
cache directory, located in `getOption("chewie.find.gedi.cache")`.

`chewie_clear_h5_temp_cache` deletes the cached .h5 files in the GEDI
h5 - these files sometimes persist when a download in incomplete or
there has been an error with the download. If you are having trouble
downloading data, running this command could well help.

`chewie_clear_parquet_cache` deletes the cached .parquet files in the
GEDI parquet cache directory, located in
`getOption("chewie.parquet.cache")`.

This function should almost never be used. Situations where this may be
useful might include - requiring to free up disk space, if you are
experiencing issues with the cache, or if there are package updates
which render the existing cache irrelevant or incompatible.
