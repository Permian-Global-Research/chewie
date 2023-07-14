#---- pretty printing ----
#' printing styles
#' @param x string to be printed
#' @noRd
chew_bold_green <- function(x) {
    crayon::green(crayon::bold(x))
}

chew_bold_red <- function(x) {
    crayon::red(crayon::bold(x))
}

chew_bold_mag <- function(x) {
    crayon::magenta(crayon::bold(x))
}

chew_bold_cyan <- function(x) {
    crayon::cyan(crayon::bold(x))
}

# ---- abort ----

abort_netrc_gen <- function(x) {
    cli::cli_abort(c(
        x,
        ">" = "Check the '.netrc' file(path) or rebuild it with:
        `chewie::chewie_creds(.force=TRUE)`",
        "i" = "The .netrc should have the following format:",
        "machine urs.earthdata.nasa.gov",
        "login [USERNAME]",
        "password [PASSWORD]"
    ))
}
#' @noRd
abort_netrc_val <- function() {
    abort_netrc_gen(
        "Your .netrc file is not formatted correctly."
    )
}
#' @noRd
abort_netrc_no_exist <- function() {
    abort_netrc_gen(
        "The porvided .netrc file does not exist."
    )
}

#' @noRd
abort_netrc_exists <- function(x) {
    cli::cli_abort(c(
        ".netrc file already exists at:",
        chew_bold_red(x),
        "i" = paste0(
            "Use ", chew_bold_cyan("`.force=TRUE`"),
            " to overwrite it."
        )
    ))
}
#' @noRd
abort_netrc_env_exists <- function(x) {
    cli::cli_abort(c(
        "`CHEWIE_NETRC` environment variable is already set to:",
        chew_bold_red(x),
        "i" = paste0(
            "Use ", chew_bold_cyan("`.force=TRUE`"),
            " to overwrite it."
        )
    ))
}

abort_non_interactive_creds <- function() {
    cli::cli_abort(c(
        "Password and username must be provided in non-interactive mode.",
        "i" = "When running `chewie::chewie_creds()` in non-interactive mode,",
        " " = paste0(
            "you must use the",
            chew_bold_mag("`.usr`"),
            "and",
            chew_bold_cyan("`.pwd`"),
            "arguments."
        )
    ))
}

#' @noRd
abort_reg <- function() {
    cli::cli_abort(c(
        "Exiting... To use `chewie` you need to create an account at:",
        chew_bold_cyan("https://urs.earthdata.nasa.gov/users/new")
    ))
}

#' @noRd
abort_gedi_request <- function(.err) {
    cli::cli_abort(c(
        "The following error occurred during the GEDI search request:",
        "i" = chew_bold_red(.err)
    ))
}

#' @noRd
abort_numeric_bbox <- function(x) {
    cli::cli_abort(c(
        "Incorrect number of coordinates for numeric chewie_bbox.",
        "i" = "x must be a numeric vector of length 4 not {length(x)}"
    ))
}

abort_date_range <- function() {
    cli::cli_abort(
        c("The provided date range is invaid!",
            "i" = paste0(
                "Make sure that `date_start` and/or `date_end` are",
                " provided as either a valid ",
                chew_bold_mag("POSIXCt"),
                " or chacter with the form ", chew_bold_mag("'YYYY-MM-DD'.")
            )
        )
    )
}

abort_no_mapview <- function() {
    cli::cli_abort(c(
        "The `mapview` package is required to use `chewie_show()`.",
        "i" = "Please install `mapview` with:",
        " " = chew_bold_cyan("`install.packages('mapview')`.")
    ))
}

# ---- inform ----
#' @noRd
inform_reg_account <- function() {
    cli::cli_inform(
        c(
            paste0(
                chew_bold_green("→ "),
                "Opening https://urs.earthdata.nasa.gov/users/new"
            ),
            "i" = "Please create an account and rerun `chewie::chewie_creds()`."
        )
    )
    invisible(suppressMessages(
        browseURL("https://urs.earthdata.nasa.gov/users/new")
    ))

    return(FALSE)
}

inform_env_success <- function(x, .quiet) {
    if (isFALSE(.quiet)) {
        cli::cli_inform(c(
            "v" = "NASA Earthdata Login Credentials Set!",
            "i" = "The '.netrc' file is located here: {x}"
        ))
    }
}


#---- warn ----
chewie_show_warn <- function(x) {
    cli::cli_alert_warning(
        c(
            paste0(
                "No `chewie_show()` method for class ",
                chew_bold_red(class(x))
            ),
            "i" = "Only objects of class `chewie.*` are supported."
        )
    )
}
