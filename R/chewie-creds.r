#' @title Test NASA Earthdata Credentials
#' @description Test NASA Earthdata credentials for use in `chewie` functions.
#' @param .netrc character path to an existing `.netrc` file.
#' @param .error logical whether to throw an error if the credentials are
#' invalid.
#' @noRd
chewie_test_creds <- function(.netrc = chewie_get_env(), .error = TRUE) {
    test_url <- paste0(
        "https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2022.12.03/",
        "GEDI02_A_2022337234828_O22520_03_T07992_02_003_02_V002.h5"
    )

    gedi_handle <- curl::new_handle(
        range = "0-1",
        netrc = TRUE,
        netrc_file = .netrc
    )

    sc <- curl::curl_fetch_memory(test_url, handle = gedi_handle)$status_code
    cred_result <- dplyr::case_when(
        sc == 206 ~ "success",
        sc == 401 ~ "unauthorized",
        TRUE ~ "ambiguous"
    )

    if (isTRUE(.error)) {
        una <- function() {
            cli::cli_abort(c("Invalid NASA Earthdata credentials!",
                "i" = "Run `chewie::chewie_creds()` to update your credentials."
            ))
        }
    } else {
        una <- function() {
            cli::cli_inform(c(
                "x" = "Invalid NASA Earthdata credentials!",
                "i" = "Run `chewie::chewie_creds()` to update your credentials."
            ))
        }
    }

    switch(cred_result,
        "success" = cli::cli_alert_success("Credentials verified!"),
        "unauthorized" = una(),
        "ambiguous" = cli::cli_alert_warning(
            "Ambiguous credentials test response: {sc}"
        )
    )
}



#' @title Validate `.netrc` file
#' @param .netrc character path to an existing `.netrc` file.
#' @noRd
chewie_validate_netrc <- function(.netrc, .test = TRUE) {
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
        return(TRUE)

        if (.test) {
            chewie_test_creds(.netrc)
        }
    } else {
        abort_netrc_no_exist()
    }
}

#' @title Create default `.netrc` file location
#' @noRd
chewie_default_dir <- function() {
    usr <- c(
        Sys.getenv("USERPROFILE", unset = NA),
        Sys.getenv("HOME", unset = NA)
    )
    .dir <- file.path(usr[which(!is.na(usr))[1]], ".chewie")

    if (!dir.exists(.dir)) {
        dir.create(.dir)
    }

    return(.dir)
}

#' @title User interactive input for NASA Earthdata Login credentials
#' @param .path character path for the new `.netrc` file.
#' @param .usr character NASA Earthdata username.
#' @param .pwd character NASA Earthdata password.
#' @noRd
chewie_write_netrc <- function(.path, .usr, .pwd) {
    netrc_conn <- file(.path)

    if (is.null(.usr)) {
        .usr <- readline(
            chew_bold_green("Enter NASA Earthdata Login Username:")
        )
    }

    if (is.null(.pwd)) {
        .pwd <- getPass::getPass(
            chew_bold_cyan("Enter NASA Earthdata Login Password:")
        )
    }

    writeLines(c(
        "machine urs.earthdata.nasa.gov",
        paste0(
            "login ",
            .usr
        ),
        paste0(
            "password ",
            .pwd
        )
    ), netrc_conn)

    close(netrc_conn)
}

#' @title Register NASA Earthdata Login Account
#' @param .netrc character path to an existing `.netrc` file.
#' @noRd
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


#' @title Set NASA Earthdata Credentials
#' @description Set NASA Earthdata credentials for use in `chewie` functions.
#' @param .netrc character path to an existing `.netrc` file.
#' @param .path character path to set where to create the `.netrc` file
#' @param .force logical whether to overwrite an existing `.netrc` or
#' `CHEWIE_NETRC` environment variable.
#' @param .usr character NASA Earthdata username.
#' @param .pwd character NASA Earthdata password.
#' @param .quiet logical whether to suppress the registration prompt and
#' messaging.
#' @rdname chewie-credentials
#' @family manage credentials
#' @export
#'
#' @details
#' The NASA Earthdata API requires a username and password to access data. This
#' is provided via the config settings in the form of a `.netrc` file. The
#' `chewie_creds` function can help to generate this file and set its location
#' as an environment variable for use in `chewie` functions. In theory this
#' should only need to be done once, but if you need to change your credentials
#' you can use the `force` argument to overwrite the existing `.netrc` file.
#'
chewie_creds <- function(
    .netrc = NULL,
    .path = NULL,
    .force = FALSE,
    .usr = NULL,
    .pwd = NULL,
    .quiet = FALSE,
    .test = TRUE) {
    if (is.null(.netrc)) {
        if (isFALSE(.quiet) && interactive()) {
            is_reg <- chewie_register()
            if (isFALSE(is_reg)) {
                return(invisible())
            }
        }
    }

    if (!is.null(.netrc)) {
        if (chewie_validate_netrc(.netrc, .test)) {
            chewie_set_env(.netrc)
            inform_env_success(.netrc, .quiet)
            return(invisible())
        }
    }

    if (is.null(.path)) {
        .path <- file.path(chewie_default_dir(), ".netrc")
    }

    if (file.exists(.path)) {
        if (isFALSE(.force)) {
            abort_netrc_exists(.path)
        }
    }

    if (!is.na(chewie_get_env())) {
        if (isFALSE(.force)) {
            abort_netrc_env_exists(chewie_get_env())
        }
    }

    if (!interactive()) {
        if (!any(c(is.null(.usr), is.null(.pwd)))) {
            abort_non_interactive_creds()
        }
    }


    chewie_write_netrc(.path, .usr, .pwd)
    chewie_set_env(.path)
    chewie_validate_netrc(.path, .test)
    inform_env_success(.path, .quiet)
}

#' @title Get NASA Earthdata Credentials environment
#' @rdname chewie-credentials
#' @family manage credentials
#' return character file path for `.netrc` file
#' @export
#' @details `chewie_get_env` can be used to manually get the `CHEWIE_NETRC`
#' environment, providing the file path to the `.netrc` file.
chewie_get_env <- function(.netrc) {
    Sys.getenv("CHEWIE_NETRC", unset = NA)
}

#' @title Remove NASA Earthdata Credentials environment variable
#' @rdname chewie-credentials
#' @family manage credentials
#' @export
#' @details `chewie_env_clean` can be used to manually remove the `CHEWIE_NETRC`
#' environment and delete the associated `.netrc` file.
chewie_env_clean <- function(renviron = "global", .check = interactive()) {
    check_n_del <- function(x) {
        if (file.exists(x)) {
            file.remove(x)
        }
    }
    clean_up <- function() {
        check_n_del(chewie_get_env())
        check_n_del(file.path(chewie_default_dir(), ".netrc"))
        if (!is.na(chewie_get_env())) {
            remove_env_var("CHEWIE_NETRC", renviron)
        }
        return(invisible())
    }

    cli::cli_inform(
        paste0(
            chew_bold_mag("?"),
            paste0(
                "   Do you really want to clear your saved NASA",
                "Earthdata login account credentials?"
            )
        )
    )

    choice <- menu(c(
        chew_bold_green("Yes"),
        chew_bold_red("No!")
    ))

    switch(choice,
        clean_up(),
        return(invisible())
    )
}


#' @title Set NASA Earthdata Credentials environment
#' @param .netrc character path to an existing `.netrc` file.
#' @param renviron character either 'global', 'local' or path to the directory
#' containing the `.Renviron` file to set the `CHEWIE_NETRC` environment.
#' @rdname chewie-credentials
#' @family manage credentials
#' @export
#' @details `chewie_set_env` is most likely not required but can be used to
#' manually set the `CHEWIE_NETRC` environment variable which is used for
#' authenticating downloads from the NASA Earthdata API.
chewie_set_env <- function(.netrc, renviron = "global") {
    add_env_var("CHEWIE_NETRC", .netrc, renviron)
}




read_renv <- function(renviron) {
    # Get the .Renviron on their system
    if (tolower(renviron) == "global") {
        home <- Sys.getenv("HOME")
    } else if (tolower(renviron) == "local") {
        home <- getwd()
    } else {
        if (dir.exists(renviron)) {
            home <- renviron
        } else {
            cli::cli_abort(".Renviron parent directory does not exist!")
        }
    }

    renv_path <- file.path(home, ".Renviron")

    con <- file(renv_path, open = "r+")
    lines <- as.character()
    ii <- 1

    while (TRUE) {
        line <- readLines(con, n = 1, warn = FALSE)
        if (length(line) == 0) {
            break()
        }
        lines[ii] <- line
        ii <- ii + 1
    }

    return(list(
        con = con,
        lines = lines,
        renv = renv_path
    ))
}
