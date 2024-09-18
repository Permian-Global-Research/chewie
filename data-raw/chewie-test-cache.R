options("chewie.testing" = TRUE)

if (!file.exists(".Renviron")) {
  file.create(".Renviron")
}

library(devtools)
devtools::load_all()

chewie_setup_cache("data-raw/chewie-test-cache", renviron = "project")
chewie_health_check()

# chewie::chewie_clear_h5_temp_cache()
# chewie::chewie_clear_find_cache()
# chewie::chewie_clear_parquet_cache("2B")
# chewie::chewie_clear_parquet_cache("2A")
# chewie::chewie_clear_parquet_cache("1B")
# chewie::chewie_clear_parquet_cache("4A")


# chewie_clear_find_cache()

# --- Download test GEDI data -------------------------------------------------
prairie_creek <- system.file(
  "geojson", "prairie-creek.geojson",
  package = "chewie"
) |>
  sf::read_sf()


get_raw_test_files <- function(gedi_prod) {
  pc_find <- find_gedi(prairie_creek,
    gedi_product = gedi_prod,
    date_start = "2023-01-01",
    date_end = "2023-01-31",
    intersects = TRUE,
    cache = TRUE
  )
  # let's forget about the h5 files for now.
  grab_gedi(pc_find, delete_h5 = TRUE)
}

gedi_products <- c("1B", "2A", "2B", "4A")

purrr::walk(
  gedi_products, get_raw_test_files
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


file.copy(gfinds, cds$find_gedi)

# clip and move parquet cache.

clip_n_save_parquet <- function(x) {
  x <- readRDS(x)

  gp <- chewie:::find_gedi_product(x)
  s_id <- x$id

  xc <- x |>
    chewie:::open_gedi() |>
    dplyr::select(!date_time) |>
    dplyr::collect()

  if (gp == "1B") {
    three_beams <- sort(unique(xc$beam))[1:3]

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

chewie_setup_cache("inst/chewie-test-cache", renviron = "project")

# for now delete the h5 directory.
unlink(getOption("chewie.h5.cache"), recursive = TRUE)

zip_path <- paste0(chewie_get_cache(), ".zip")
if (file.exists(zip_path)) {
  file.remove(zip_path)
}
zip::zip(paste0(chewie_get_cache(), ".zip"),
  chewie_get_cache(),
  mode = "cherry-pick"
)

unlink(chewie_get_cache(), recursive = TRUE)
