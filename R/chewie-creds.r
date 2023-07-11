chewie_validate_netrc <- function(.netrc) {
    if (file.exists(.netrc)) {
        nrc_vals <- readLines(.netrc) |>
            strsplit(" ") |>
            unlist()

        check_list <- c(
            nrc_vals[1] == "machine",
            nrc_vals[2] == "urs.earthdata.nasa.gov",
            nrc_vals[3] == "login",
            nrc_vals[5] == "password",
            length(nrc_vals) == 6
        )

        if (!all(check_list)) {
            abort_netrc_val()
        }
        return(FALSE)
    } else {
        return(TRUE)
    }
}




chewie_write_netrc <- function(.path = NULL, .force = FALSE) {
    if (isTRUE(.force)) {
        netrc_conn <- file(.netrc)

        writeLines(c(
            "machine urs.earthdata.nasa.gov",
            paste0(
                "login ",
                readline(
                    prompt =
                        chew_bold_green("Enter NASA Earthdata Login Username:")
                )
            ),
            paste0(
                "password ",
                getPass::getPass(
                    msg =
                        chew_bold_cyan("Enter NASA Earthdata Login Password:")
                )
            )
        ), netrc_conn)
        close(netrc_conn)
    }
    return(.netrc)
}



chewie_default_netrc <- function() {
    usr <- c(
        Sys.getenv("USERPROFILE", unset = NA),
        Sys.getenv("HOME", unset = NA)
    )
    .dir <- file.path(usr[which(!is.na(usr))[1]], ".chewie")

    if (!dir.exists(.dir)) {
        dir.create(.dir)
    }

    return(file.path(.dir, ".netrc"))
}

chewie_set_netrc <- function(.path = NULL, .quiet = FALSE, .force = FALSE) {
    if (!is.null(.netrc)) {
        Sys.setenv(CHEWIE_NETRC = .netrc)
    } else {
        chewie_netrc <- Sys.getenv("CHEWIE_NETRC", unset = NA)

        if (!is.na(chewie_netrc)) {
            if (isTRUE(.force)) {
                cli::cli_alert_warning(c(
                    "Overwriting existing CHEWIE_NETRC environment var to ",
                    chew_bold_mag(chewie_default_netrc())
                ))
            } else {
                if (!.quiet) {
                    cli::cli_alert_info(c(
                        "Using existing CHEWIE_NETRC environment var:",
                        chew_bold_mag(chewie_netrc)
                    ))
                }
                return(chewie_netrc)
            }
        }

        if (!.quiet) {
            cli::cli_alert_info(paste0(
                chew_bold_cyan("→ "),
                "Setting CHEWIE_CREDS environment to ",
                chew_bold_green(chewie_default_netrc())
            ))
        }

        chewie_netrc <- chewie_default_netrc()

        Sys.setenv(CHEWIE_CREDS = )
    }
}



chewie_register <- function(.netrc) {
    cli::cli_inform(paste0(
        chew_bold_cyan("?"),
        "  Do You Have a NASA Earthdata Login Account?"
    ))
    choice <- menu(c(
        chew_bold_green("Yes"),
        chew_bold_mag("No, but I want to create an account"),
        chew_bold_red("No, and I don't want one!")
    ))

    switch(choice,
        TRUE,
        inform_reg_account(),
        abort_reg()
    )
}

chewie_set_env <- function(.netrc) {
    Sys.setenv(CHEWIE_NETRC = .netrc)
}


chewie_creds <- function(
    .netrc = NULL,
    .path = NULL,
    .force = FALSE,
    .quiet = FALSE) {
    if (is.null(.netrc) || isTRUE(.quiet)) {
        is_reg <- chewie_register()
        if (isFALSE(is_reg)) {
            return(invisible())
        }
    }

    if (!is.null(.netrc)) {
        if (isFALSE(chewie_validate_netrc(.netrc))) {
            chewie_set_env(.netrc)
            inform_env_success(.netrc)
        }
    }


    message("what??")
}
