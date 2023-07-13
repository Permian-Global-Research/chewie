#' GEDI finder
#'
#' @description This function finds the exact granule(s) that contain GEDI data
#' for a given region of interest and date range
#'
#' @param product GEDI data level; Options: "GEDI01_B", "GEDI02_A" or "GEDI02_B"
#' @param ul_lat Numeric. Upper left (ul) corner coordinates, in lat
#' (decimal degrees) for the bounding box of the area of interest.
#' @param ul_lon Numeric. Upper left (ul) corner coordinates, in lon
#' (decimal degrees) for the bounding box of the area of interest.
#' @param lr_lat Numeric. Lower right (ul) corner coordinates, in lat
#' (decimal degrees) for the bounding box of the area of interest.
#' @param lr_lon Numeric. Lower right (ul) corner coordinates, in lon
#' (decimal degrees) for the bounding box of the area of interest.
#' @param version Character. The version of the GEDI product files to be
#' returned. Default "002".
#' @param daterange Vector. Date range. Specify your start and end dates
#' using ISO 8601 \[YYYY\]-\[MM\]-\[DD\]T\[hh\]:\[mm\]:\[ss\]Z. Ex.:
#' c("2019-07-01T00:00:00Z","2020-05-22T23:59:59Z"). If NULL (default),
#' the date range filter will be not applied.
#'
#' @return Return a vector object pointing out the path saving the downloaded
#' GEDI data within the boundary box coordinates provided
#'
#' @seealso bbox: Defined by the upper left and lower right corner coordinates,
#' in lat,lon ordering, for the bounding box of the area of interest
#' (e.g. \[ul_lat,ul_lon,lr_lat,lr_lon\]).
#'
#' This function relies on the existing CMR tool:
#' \url{https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html}
#'
#' @examples
#' \donttest{
#' # gedifinder is a web service provided by NASA
#' # usually the request takes more than 5 seconds
#'
#' # Specifying bounding box coordinates
#' ul_lat <- 42.0
#' ul_lon <- -100
#' lr_lat <- 40.0
#' lr_lon <- -96.0
#'
#' # Specifying the date range
#' daterange <- c("2019-07-01", "2020-05-22")
#'
#' # Extracting the path to GEDI data for the specified boundary box coordinates
#' gedi02b_list <- gedifinder(
#'     product = "GEDI02_B",
#'     ul_lat,
#'     ul_lon,
#'     lr_lat,
#'     lr_lon,
#'     version = "002",
#'     daterange = daterange
#' )
#' }
#' @import jsonlite curl
#' @export
chewie_find <- function(
    x,
    product,
    gedi_version = "002",
    daterange = NULL) {
    concept_ids <- list(
        GEDI01_B.001 = "C1656765475-LPDAAC_ECS",
        GEDI02_A.001 = "C1656766463-LPDAAC_ECS",
        GEDI02_B.001 = "C1656767133-LPDAAC_ECS",
        GEDI01_B.002 = "C1908344278-LPDAAC_ECS",
        GEDI02_A.002 = "C1908348134-LPDAAC_ECS",
        GEDI02_B.002 = "C1908350066-LPDAAC_ECS"
    )
    page <- 1
    # bbox <- paste(ul_lon, lr_lat, lr_lon, ul_lat, sep = ",")
    bbox <- paste(chewie_bbox(x), collapse = ",")
    # Granules search url pattern
    url_format <- paste0(
        "https://cmr.earthdata.nasa.gov/search/granules.json?",
        "pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id=%s",
        "&bounding_box=%s"
    )
    request_url <- sprintf(
        url_format,
        concept_ids[paste0(product, ".", gedi_version)],
        bbox
    )

    # Add temporal search if not null
    if (!is.null(daterange)) {
        url_format <- paste0(request_url, "&temporal=%s,%s")
        request_url <- sprintf(url_format, daterange[1], daterange[2])
    }

    sf_list <- list()

    repeat {
        gedi_response <- request_gedi(request_url, page)
        if (length(gedi_response) == 0) break
        sf_polygon <- build_sf_set(gedi_response)
        sf_list[[page]] <- sf_polygon
        page <- page + 1
    }

    return(sf_rbindlist(sf_list))
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
                wk::xy(coords[c(TRUE, FALSE)], coords[c(FALSE, TRUE)]),
                wk::wk_polygon_filter(
                    wk::sfc_writer()
                )
            ) |>
                sf::st_as_sf(crs = "EPSG:4326")


            poly$url <- x$links[[1]]$href
            poly$time_start <- x$time_star
            poly$time_end <- x$time_end
            poly$id <- x$id
            return(poly)
        }
    ) |>
        sf_rbindlist()
}
