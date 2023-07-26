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
#' @details
#' Where x is a numeric it must be of length 4 with values corresponding to the
#' bounding box coordinates in the order xmin, ymin, xmax, ymax.
#'
#' @export
chewie_find <- function(
    x,
    gedi_product = c("1B", "2A", "2B"),
    gedi_version = c("v2", "v1"),
    date_start = NULL,
    date_end = NULL,
    intersects = TRUE) {
    bbox <- paste(chewie_bbox(x), collapse = ",")

    date_range <- build_date_range(date_start, date_end)

    request_url <- build_req_url(
        gedi_product[1],
        gedi_version[1],
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

    sf_list <- sf_rbindlist(sf_list)
    data.table::setcolorder(
        sf_list,
        c("id", "time_start", "time_end", "url", "geometry")
    )


    if (isTRUE(intersects)) {
        sf_list <- get_swath_intersect(x, sf_list)
    }

    attr(sf_list, "aoi") <- get_spat_outline(x)
    attr(sf_list, "gedi_product") <- paste("GEDI",
        gedi_product[1], gedi_version[1],
        sep = "-"
    )
    attr(sf_list, "class") <- c("chewie.find", class(sf_list))
    return(sf_list)
}

request_gedi <- function(.url, .page) {
    # Append fetched granules to granules_href
    # recursively, for each page (max 2000 per page)
    response <- curl::curl_fetch_memory(paste0(
        .url,
        "&pageNum=",
        .page
    ))
    content <- rawToChar(response$content)
    result <- jsonify::from_json(content, simplify = FALSE)
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


gedi_code_lookup <- function(.gprod, .gver) {
    switch(paste0("G", .gprod, .gver),
        G1Bv1 = "C1656765475-LPDAAC_ECS",
        G2Av1 = "C1656766463-LPDAAC_ECS",
        G2Bv1 = "C1656767133-LPDAAC_ECS",
        G1Bv2 = "C1908344278-LPDAAC_ECS",
        G2Av2 = "C1908348134-LPDAAC_ECS",
        G2Bv2 = "C1908350066-LPDAAC_ECS",
        abort_gedi_opts()
    )
}


build_req_url <- function(.gprod, .gver, .bbox, .dr) {
    req_url <- paste0(
        "https://cmr.earthdata.nasa.gov/search/granules.json?",
        "pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id=",
        gedi_code_lookup(.gprod, .gver),
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
#' @param ... additional arguments passed to `print.data.table`
#' @export
#' @details Edit the following options to change the default printing behaviour
#' of `chewie.find` objects: `chewie.print.class`, `chewie.print.keys`,
#' `chewie.print.topn`, `chewie.print.nrows`, `chewie.print.trunc.cols`,
#' `chewie.prettyprint.char`, `chewie.print.width`. Additonal options for
#' `print.data.table` can also be set to alter the print layout.
print.chewie.find <- function(x, ...) {
    chewie_print(x, ...)
}
