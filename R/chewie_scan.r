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
    cached <- purrr::map_vec(
        swath_dirs,
        ~ length(list.files(.x, pattern = ".*\\.parquet$")) > 0
    )

    cache_tab <- data.frame(
        id = cached_swaths,
        cached = cached
    )

    # left join cache tab with x
    cache_tab <- dplyr::left_join(
        x,
        cache_tab,
        by = "id"
    ) |> dplyr::mutate(
        cached = dplyr::case_when(
            is.na(cached) ~ FALSE,
            TRUE ~ cached
        )
    )

    class(cache_tab) <- class(x)
    attr(cache_tab, "aoi") <- attr(x, "aoi")
    attr(cache_tab, "gedi_product") <- attr(x, "gedi_product")
    attr(cache_tab, "intersects") <- attr(x, "intersects")

    return(cache_tab)
}
