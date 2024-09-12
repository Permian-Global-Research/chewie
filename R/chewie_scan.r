#' @title chewie_scan
#' @description Internal function to scan the cache for existing data from the
#' GEDI find (search) function.
#' @param x A `chewie.find` object.
#' @noRd
chewie_scan <- function(x) {
  gedi_product <- find_gedi_product(x)

  parquet_dir <- file.path(
    getOption("chewie.parquet.cache"),
    gedi_product
  )

  nothing_here <- function(x) {
    reclass <- class(x)
    x$cached <- FALSE
    class(x) <- reclass
    return(x)
  }

  # get paths for existing folders
  swath_dirs <- list.dirs(parquet_dir, recursive = FALSE)
  if (length(swath_dirs) == 0) {
    return(nothing_here(x))
  }
  # extract ids from existing folders
  all_swaths <- sub(".*\\=", "", x = basename(swath_dirs))
  # get ids for swaths that are not in the cache
  req_swaths <- x$id[!x$id %in% all_swaths]
  if (length(req_swaths) == nrow(x)) {
    return(nothing_here(x))
  }

  # get paths for swaths that are in the cache
  cached_swaths <- x$id[x$id %in% all_swaths]
  # check if there are parquet files in the folders
  cached_bool <- purrr::map_vec(
    file.path(parquet_dir, paste0("granule_id=", cached_swaths)),
    ~ length(list.files(.x, pattern = ".*\\.parquet$")) > 0
  )

  cache_tab <- data.frame(
    id = cached_swaths,
    cached = cached_bool
  )

  # save classes and attributes now before transformation.
  x_class <- class(x)
  aoi_attr <- attr(x, "aoi")
  gedi_product_attr <- attr(x, "gedi_product")
  intersects_attr <- attr(x, "intersects")

  # drop `cached` column if it exists in `x`
  # left join cache tab with x
  cache_tab <- x |>
    dplyr::select(!dplyr::any_of("cached")) |>
    dplyr::left_join(
      cache_tab,
      by = "id"
    ) |>
    dplyr::mutate(
      cached = dplyr::case_when(
        is.na(cached) ~ FALSE,
        TRUE ~ cached
      )
    )

  # reassign classes and attributes
  class(cache_tab) <- x_class
  attr(cache_tab, "aoi") <- aoi_attr
  attr(cache_tab, "gedi_product") <- gedi_product_attr
  attr(cache_tab, "intersects") <- intersects_attr

  return(cache_tab)
}
