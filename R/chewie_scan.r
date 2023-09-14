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

    n_swaths <- nrow(x)

    swath_dirs <- list.dirs(parquet_dir, recursive = FALSE)
    if (length(swath_dirs) == 0) {
        reclass <- class(x)
        x$cached <- FALSE
        class(x) <- reclass
        return(x)
    }
    swath_ids <- sub(".*\\=", "", x = basename(swath_dirs))
    req_swaths <- swath_ids[swath_ids %in% x$id]
    swath_dirs <- swath_dirs[swath_ids %in% x$id]

    cached <- purrr::map_vec(
        swath_dirs,
        ~ length(list.files(.x, pattern = ".*\\.parquet$")) > 0
    )

    cache_tab <- data.frame(
        id = req_swaths,
        cached = cached
    )

    # left join cache tab with x
    cache_tab <- merge(x, cache_tab, by = "id", all.x = TRUE) |>
        sf::st_drop_geometry()
    cache_tab$cached[is.na(cache_tab$cached)] <- FALSE

    to_download <- cache_tab[!cache_tab$cached, ]

    if (nrow(to_download) == 0) {
        cli::cli_alert_success("All data found in cache")
    } else {
        if (nrow(to_download) != n_swaths) {
            cli::cli_inform(
                c(
                    "i" = "{n_swaths - nrow(to_download)} of {n_swaths}
                    file{?s}  found in cache"
                )
            )
        }
    }
    return(to_download)
}
