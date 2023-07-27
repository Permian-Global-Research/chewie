#' @title Create a snake case timestamp
#' @noRd
dt_snake_case <- function() {
    sub("\\..*", "",
        x = gsub(":", "-",
            x = gsub(" ", "-", Sys.time())
        )
    )
}
