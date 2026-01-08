# Collect waveform data from GEDI 1B data

Collect waveform data from GEDI 1B data

## Usage

``` r
extract_waveforms(x)
```

## Arguments

- x:

  A tibble or data.frame object returned from `collect_gedi`.

## Value

a tibble containing the waveform data.

## Details

This function is used to collect the waveform data from GEDI 1B data
returned from `collect_gedi`. The waveform amplitude is converted to a
float value with a precision of 0.0001 (i.e. 4 decimal places) and the
elevation is calculated from the `elevation_bin0`, `elevation_lastbin`
and `rx_sample_count` columns.

This function converts the waveform lists for each footprint to long
form and merges all shots into a single tibble. The advantage of this is
that it enables the analysis and comparison of multiple plots. If you
wish to filter the shots that you want to analyse, simply filter the
results of 'collect_gedi' before passing to this function.

## Examples

``` r
if (FALSE) { # interactive()
prairie_creek <- sf::read_sf(
  system.file("geojson", "prairie-creek.geojson", package = "chewie")
)
prairie_creek_find_1b <- find_gedi(prairie_creek,
  gedi_product = "1B",
  date_start = "2022-01-01", date_end = "2022-04-01",
  cache = FALSE
)

prairie_creek_grab_1b <- grab_gedi(prairie_creek_find_1b)

prairie_creek_1b_sf <- collect_gedi(
  prairie_creek_grab_1b,
  prairie_creek_find_1b
)

prairie_creek_waveforms <- collect_waveforms(prairie_creek_1b_sf)
print(prairie_creek_waveforms)
}
```
