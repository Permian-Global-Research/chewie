#' @title Create a snake case timestamp
#' @noRd
dt_snake_case <- function() {
    sub("\\..*", "",
        x = gsub(":", "-",
            x = gsub(" ", "-", Sys.time())
        )
    )
}

#' @title get the GEDI product from a `chewie.find` object
#'
find_gedi_product <- function(x) {
    g_prod <- sub(".*\\-", "", attributes(x)$gedi_product)
    assert_gedi_product(g_prod)
    return(g_prod)
}
