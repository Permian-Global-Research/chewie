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
#' @noRd
abort_netrc_val <- function() {
    cli::cli_abort(c(
        "Your .netrc file is not formatted correctly.",
        ">" = "Check your '.netrc' file or rebuild it with:
        `chewie::chewie_creds(.force=TRUE)`",
        "i" = "The .netrc should have the following format:",
        "machine urs.earthdata.nasa.gov",
        "login [USERNAME]",
        "password [PASSWORD]"
    ))
}

#' @noRd
abort_reg <- function() {
    cli::cli_abort(c(
        "Exiting... To use `chewie` you need to create an account at:",
        chew_bold_cyan("https://urs.earthdata.nasa.gov/users/new")
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

inform_env_success <- function(x) {
    cli::cli_inform(c(
        "v" = "NASA Earthdata Login Credentials Set!",
        "i" = "The '.netrc' file is located here: {x}"
    ))
}
