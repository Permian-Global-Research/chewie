#---- pretty printing ----
#' printing styles
#' @param x string to be printed
#' @noRd
chew_bold_green <- function(x) {
    cli::col_green(cli::style_bold(x))
}

chew_bold_red <- function(x) {
    cli::col_red(cli::style_bold(x))
}

chew_bold_mag <- function(x) {
    cli::col_magenta(cli::style_bold(x))
}

chew_bold_cyan <- function(x) {
    cli::col_cyan(cli::style_bold(x))
}

chew_bold_yel <- function(x) {
    cli::col_yellow(cli::style_bold(x))
}

# ---- abort ----

abort_netrc_gen <- function(x) {
    cli::cli_abort(c(
        x,
        ">" = "Check the '.netrc' file(path) or rebuild it with:
        `chewie::chewie_creds(force=TRUE)`",
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
            "Use ", chew_bold_cyan("`force=TRUE`"),
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
            "Use ", chew_bold_cyan("`force=TRUE`"),
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

abort_gedi_opts <- function() {
    cli::cli_abort(c("Invalid GEDI product",
        "i" = paste0(
            "Valid GEDI products are: ",
            chew_bold_cyan("1B"), ", ",
            chew_bold_cyan("2A"), ", and ",
            chew_bold_cyan("2B"), "."
        )
    ))
}

abort_env_set <- function(env_name) {
    cli::cli_abort(c(
        "Environment variable `{env_name}` is already set."
    ))
}

abort_no_gedi_data <- function() {
    cli::cli_abort(c(
        "No GEDI data found for the provided region and/or datetime range.",
        "i" = "Try expanding the search area or changing the datetime range."
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

inform_ask_env_overwrite <- function(x) {
    cli::cli_inform(c(
        "!" = "Environment variable  `{x}` is already set.",
        ">" = "Do you want to overwrite it?"
    ))
}

inform_cache_set_success <- function(x) {
    cli::cli_inform(c(
        "v" = "GEDI cache set in the following directory:",
        ">" = cli::style_italic(paste0('"', x, '"'))
    ))
}

inform_env_health <- function(x) {
    cli::cli_inform(c(
        "x" = x,
        "i" = paste0(
            "Please run `",
            chew_bold_mag("chewie::chewie_creds()"),
            "` to set up your credentials."
        )
    ))
}

inform_cache_health <- function(x) {
    cli::cli_inform(c(
        "x" = x,
        "i" = paste0(
            "Please run `",
            chew_bold_mag("chewie::chewie_setup_cache()"),
            "` to set up your cache."
        )
    ))
}

inform_time <- function(st, type) {
    tot_time <- Sys.time() - st
    time_units <- attr(tot_time, "units")
    num_time <- round(as.numeric(tot_time, units = time_units), 1)
    cli::cli_alert_info("{type} time: {num_time} {time_units}")
}

inform_download_completed <- function(ncomp, n) {
    cli::cli_alert_success(
        " {ncomp}/{n} {cli::qty(ncomp)}file{?s} already downloaded."
    )
}

#---- warn ----
chewie_show_warn <- function(x) {
    cli::cli_inform(
        c(
            "!" =
                paste0(
                    "No `chewie_show()` method for class ",
                    chew_bold_red(class(x)[1])
                ),
            "i" = "Only objects of class `chewie.*` are supported."
        )
    )
}

end_date_cache_warn <- function(x) {
    cli::cli_inform(c(
        "!" = "No end date was provided - A Non-permenant
            cache is in effect. \n",
        "i" = "The cache will be invalidated on {x}. \n",
        " " = "To establish a permanent cache set the end date using the
                `date_end` argument."
    ))
}
