#' @title Find GEDI swaths
#' @description Find GEDI swaths that intersect with a given spatial object.
#' @param x object of class `sf`, `spatVector`, `spatRaster`, `sfc`, `stars`,
#' `stars_proxy`, or `numeric` see details.
#' @param gedi_product character of GEDI product to search for.
#' @param gedi_version character of GEDI version to search for.
#' @param date_start character or `POSIXct` of the start date to search for GEDI
#' data. If `NULL` defaults to the start of GEDI operations (2019-03-25).
#' @param date_end character or `POSIXct` of the end date to search for GEDI
#' data. If `NULL` defaults to the current date.
#' @param intersects logical indicating whether to return only swaths that
#' intersect with the given spatial object. If FALSE all swaths that are
#' contained within the bounding box of the spatial object are returned.
#' @param cache logical indicating whether to cache the results of the GEDI
#' search. If TRUE the results of the search will be cached in the directory
#' defined by: `getOption("chewie.find.gedi.cache")`. If FALSE the results will
#' not be cached.
#' @rdname chewie-find-gedi
#' @family find GEDI
#' @details
#' Where x is a numeric it must be of length 4 with values corresponding to the
#' bounding box coordinates in the order xmin, ymin, xmax, ymax.
#'
#' @export
find_gedi <- function(
    x,
    gedi_product = c("1B", "2A", "2B", "4A"),
    date_start = NULL,
    date_end = NULL,
    intersects = TRUE,
    cache = TRUE) {
  rlang::arg_match(gedi_product)

  bbox <- paste(chewie_bbox(x), collapse = ",")

  date_range <- build_date_range(date_start, date_end)



  if (isTRUE(cache)) {
    if (is.null(date_end)) {
      cache_end <- lubridate::now() |>
        lubridate::add_with_rollback(months(1)) |>
        lubridate::ceiling_date(unit = "month") |>
        lubridate::format_ISO8601()
      end_date_cache_warn(cache_end)
    } else {
      cache_end <- date_end
    }
    cache_string <- paste(
      gedi_product[1],
      bbox,
      date_range[1],
      cache_end,
      intersects,
      sep = "_"
    ) |>
      gsub("[:.,]", "_", x = _)

    find_cache_dir <- getOption("chewie.find.gedi.cache")
    check_n_make_dir(find_cache_dir)

    cache_file <- file.path(
      find_cache_dir,
      paste0(cache_string, ".rds")
    )
    if (file.exists(cache_file)) {
      cli::cli_alert_success("Using cached GEDI data")
      cached_find <- readRDS(cache_file)
      return(chewie_scan(cached_find))
    }
  }



  request_url <- build_req_url(
    gedi_product[1],
    bbox,
    date_range
  )

  sf_list <- list()
  page <- 1
  repeat {
    gedi_response <- request_gedi(request_url, page)
    if (length(gedi_response) == 0) break
    sf_polygon <- build_sf_set(gedi_response)
    sf_list[[page]] <- sf_polygon
    page <- page + 1
  }

  if (length(sf_list) == 0) {
    abort_no_gedi_data()
  }


  sf_list <- sf_rbindlist(sf_list)
  data.table::setcolorder(
    sf_list,
    c("id", "time_start", "time_end", "url", "geometry")
  )


  if (isTRUE(intersects)) {
    sf_list <- get_swath_intersect(x, sf_list)
  }

  attr(sf_list, "aoi") <- get_spat_outline(x)
  attr(sf_list, "gedi_product") <- paste0("GEDI-", gedi_product[1])
  attr(sf_list, "intersects") <- intersects
  attr(sf_list, "class") <- c(
    "chewie.find",
    paste("chewie.find", "gedi", gedi_product[1], sep = "."),
    class(sf_list)
  )

  if (isTRUE(cache)) {
    saveRDS(sf_list, cache_file)
  }

  return(chewie_scan(sf_list))
}

request_gedi <- function(.url, .page) {
  response <- curl::curl_fetch_memory(paste0(
    .url,
    "&pageNum=",
    .page
  ))

  content <- rawToChar(response$content)
  result <- RcppSimdJson::fparse(content, max_simplify_lvl = "list")
  if (response$status_code != 200) {
    abort_gedi_request(result$errors)
  }
  return(result$feed$entry)
}

build_sf_set <- function(gedi_response) {
  lapply(
    gedi_response,
    function(x) {
      coords <- unlist(x$polygons) |>
        strsplit(" ") |>
        unlist() |>
        as.numeric()

      poly <- wk::wk_handle(
        wk::xy(x = coords[c(FALSE, TRUE)], y = coords[c(TRUE, FALSE)]),
        wk::wk_polygon_filter(
          wk::sfc_writer()
        )
      ) |>
        sf::st_as_sf(crs = "EPSG:4326")


      poly$url <- x$links[[1]]$href
      poly$time_start <- lubridate::as_datetime(x$time_star)
      poly$time_end <- lubridate::as_datetime(x$time_end)
      poly$id <- x$id
      return(poly)
    }
  ) |>
    sf_rbindlist()
}


gedi_code_lookup <- function(.gprod) {
  switch(paste0("G", .gprod),
    G1B = "C1908344278-LPDAAC_ECS",
    G2A = "C1908348134-LPDAAC_ECS",
    G2B = "C1908350066-LPDAAC_ECS",
    G4A = "C2237824918-ORNL_CLOUD",
    abort_gedi_opts()
  )
}


build_req_url <- function(.gprod, .bbox, .dr) {
  .provider <- ifelse(.gprod == "4A", "ORNL_CLOUD", "LPDAAC_ECS")
  req_url <- paste0(
    "https://cmr.earthdata.nasa.gov/search/granules.json?",
    "pretty=true&provider=",
    .provider,
    "&page_size=2000&concept_id=",
    gedi_code_lookup(.gprod),
    "&bounding_box=",
    .bbox
  )

  if (!is.null(.dr)) {
    time_snip <- sprintf("&temporal=%s,%s", .dr[1], .dr[2])
    req_url <- paste0(req_url, time_snip)
  }

  return(req_url)
}


build_date_range <- function(.sd, .ed) {
  if (is.null(.sd) && is.null(.ed)) {
    return(NULL)
  } else if (is.null(.ed)) {
    .ed <- lubridate::format_ISO8601(lubridate::now())
    .sd <- lubridate::format_ISO8601(lubridate::as_datetime(.sd))
  } else if (is.null(.sd)) {
    .sd <- lubridate::format_ISO8601(
      lubridate::as_datetime("2019-03-25") # GEDI official ops start
    )
    .ed <- lubridate::format_ISO8601(lubridate::as_datetime(.ed))
  }
  .dr <- c(.sd, .ed)
  if (any(is.na(.dr))) {
    abort_date_range()
  }


  return(c(.sd, .ed))
}

#' @title print a `chewie.find` object
#' @description `print.chewie.find` is a method for printing `chewie.find`
#' objects. Essentially a wrapper for `print.data.table` with some custom
#' options. See details for options.
#' @param x chewie.find object to print
#' @param ... arguments passed to
#' \code{\link[data.table:print.data.table]{data.table::print.data.table}}
#' @rdname chewie-print
#' @family print
#' @export
#' @details Edit the following options to change the default printing behaviour
#' of `chewie.find` objects: `chewie.print.class`, `chewie.print.keys`,
#' `chewie.print.topn`, `chewie.print.nrows`, `chewie.print.trunc.cols`,
#' `chewie.prettyprint.char`, `chewie.print.width`. Additonal options for
#' `print.data.table` can also be set to alter the print layout.
#'
print.chewie.find <- function(x, ...) {
  chewie_print(x, ...)
}

#' @title plot a `chewie.find` object
#' @description `plot.chewie.find` is a method for plotting `chewie.find`
#' objects.
#' @param x chewie.find object to print
#' @param swath_col character; color of the swaths.
#' @param aoi_col character; color of the aoi.
#' @param ... arguments passed to `print.sf`
#' @return a base R plot.
#' @export
plot.chewie.find <- function(x, swath_col = "#903ca586", aoi_col = "#cecece", ...) {
  plot(attributes(x)$aoi, col = aoi_col, axes = TRUE)
  plot(x[0], col = swath_col, add = TRUE)
}

#' @title Clear the GEDI find (search) cache
#' @rdname chewie-find-gedi
#' @family find GEDI.
#' @details
#' `chewie_clear_find_cache` deletes the cached .rds files in the GEDI find
#' cache directory, located in `getOption("chewie.find.gedi.cache")`.
#' @export
chewie_clear_find_cache <- function() {
  cache_dir <- getOption("chewie.find.gedi.cache")

  clean_finds <- function() {
    list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE) |>
      purrr::walk(file.remove)
  }

  cli::cli_inform(
    paste0(
      chew_bold_mag("?"),
      paste0(
        "   Do you really want to clear your GEDI search cache?"
      )
    )
  )

  choice <- menu(c(
    chew_bold_green("Yes"),
    chew_bold_red("No!")
  ))

  switch(choice,
    clean_finds(),
    return(invisible())
  )
}
