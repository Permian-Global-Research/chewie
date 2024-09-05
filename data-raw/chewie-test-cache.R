if (!file.exists(".Renviron")) {
  file.create(".Renviron")
}

# quit()

library(devtools)
devtools::load_all()
library(furrr)

chewie_setup_cache("data-raw/chewie-test-cache", renviron = "project")
chewie_health_check()
chewie_get_cache()
# chewie_clear_find_cache()
# chewie_clear_parquet_cache("2A")
# chewie_creds()
# chewie_creds(netrc = "/home/hugh/.chewie/.netrc")

# --- Download test GEDI data -------------------------------------------------
prairie_creek <- system.file(
  "geojson", "prairie-creek.geojson",
  package = "chewie"
) |>
  sf::read_sf()


# --- more dev first:

chewie::chewie_clear_parquet_cache("2B")

pc_find <- find_gedi(prairie_creek,
  gedi_product = "2B",
  date_start = "2023-01-01",
  date_end = "2023-01-31",
  intersects = TRUE,
  cache = TRUE
)

x <- grab_gedi(pc_find, delete_h5 = FALSE)

y <- chewie::collect_gedi(x, pc_find) |>
  dplyr::mutate(
    pai_z5_10m = dplyr::case_when(
      pai_z5_10m == -9999 ~ NA,
      .default = pai_z5_10m
    )
  )

chewie_show(y, zcol = "pai_z5_10m")

dplyr::collect(x)

dim(x)



get_raw_test_files <- function(gedi_prod) {
  pc_find <- find_gedi(prairie_creek,
    gedi_product = gedi_prod,
    date_start = "2023-01-01",
    date_end = "2023-01-31",
    intersects = TRUE,
    cache = TRUE
  )

  grab_gedi(pc_find, delete_h5 = FALSE)
}

gedi_products <- c("1B", "2A", "2B", "4A")
future::plan("multisession", workers = length(gedi_products))

furrr::future_walk(
  gedi_products, get_raw_test_files,
  .options = furrr::furrr_options(seed = 5446)
)

# ----- copy reduced data to inst -------------

make_inst_cache <- function(.dir = "inst/chewie-test-cache") {
  dir_parquet <- file.path(.dir, "GEDI-parquet-cache")
  dir_h5 <- file.path(.dir, "GEDI-h5-cache-temp")
  di_find_gedi <- file.path(.dir, "find-gedi-cache")

  check_n_make_dir(dir_parquet)
  check_n_make_dir(dir_h5)
  check_n_make_dir(di_find_gedi)
  return(list(
    parquet = dir_parquet,
    h5 = dir_h5,
    find_gedi = di_find_gedi
  ))
}

# move find gedi cache
cds <- make_inst_cache()

gfinds <- list.files(getOption("chewie.find.gedi.cache"),
  recursive = TRUE, full.names = TRUE
)

getOption("chewie.find.gedi.cache")
getOption("chewie.parquet.cache")
getOption("chewie.h5.cache")


file.copy(gfinds, cds$find_gedi)

# clip and move parquet cache.

clip_n_save_parquet <- function(x) {
  x <- readRDS(x)

  gp <- chewie:::find_gedi_product(x)
  s_id <- x$id
  # browser()
  xc <- x |>
    chewie:::open_gedi() |>
    dplyr::select(!date_time) |>
    dplyr::collect()

  if (gp == "1B") {
    three_beams <- sort(unique(xc$beam))[1:3]
    # browser()
    xc <- xc |>
      dplyr::filter(beam %in% three_beams)
  }

  save_dir <- file.path(
    cds$parquet,
    gp,
    paste0("granule_id=", s_id)
  )


  chewie:::check_n_make_dir(save_dir)

  arrow::write_parquet(
    dplyr::as_tibble(xc),
    file.path(
      save_dir,
      paste0(
        tools::file_path_sans_ext(basename(x$url)),
        ".parquet"
      )
    )
  )
}

purrr::walk(gfinds, clip_n_save_parquet)




mapview::mapview(xc |>
  dplyr::select(!rxwaveform) |>
  sf::st_as_sf(coords = c("longitude_bin0", "latitude_bin0")) |>
  sf::st_set_crs(4326))






# pc_find <- find_gedi(prairie_creek,
#   gedi_product = "1B",
#   date_start = "2023-01-01",
#   date_end = "2023-01-31",
#   intersects = TRUE,
#   cache = FALSE
# )

# pc_grab <- grab_gedi(pc_find, delete_h5 = FALSE)

# pc_sf <- pc_grab |>
#   dplyr::filter(
#     quality_flag == 1,
#     degrade_flag == 0
#   ) |>
#   collect_gedi(gedi_find = pc_find)

# chewie_show(pc_grab,
#   zcol = "rh95", zoom = 11
# )


chewie_setup_cache("inst/chewie-test-cache", renviron = "project", quiet = TRUE)
chewie_health_check()
chewie_get_cache()
