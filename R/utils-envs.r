#' @title Add environment variable to `.Renviron`
#' @param env_name character name of the environment variable to add.
#' @param env_val character value of the environment variable to add.
#' @param renviron character either 'global', 'local' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
add_env_var <- function(env_name, env_val, renviron = "global") {
    if (isTRUE(check_env_var(env_name, renviron))) {
        if (!interactive()) {
            abort_env_set(env_name)
        }

        inform_ask_env_overwrite(env_name)

        choice <- menu(c(
            chew_bold_green("Yes"),
            chew_bold_red("No, are you mad?!")
        ))

        switch(choice,
            remove_env_var(env_name, renviron),
            return(invisible())
        )
    }

    rr <- read_renv(renviron)

    system_vars <- c(rr$lines, paste0(env_name, " = ", '"', env_val, '"'))
    writeLines(system_vars, rr$con)
    on.exit(close(rr$con), add = TRUE)

    ev <- list(env_val)
    names(ev) <- env_name
    do.call(Sys.setenv, ev)
}

#' @title Remove environment variable from `.Renviron`
#' @param env_name character name of the environment variable to remove.
#' @param renviron character either 'global', 'local' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
remove_env_var <- function(env_name, renviron = "global") {
    rr <- read_renv(renviron)
    on.exit(close(rr$con), add = TRUE)
    system_vars <- rr$lines[!grepl(env_name, rr$lines)]
    file_con <- file(rr$renv)
    writeLines(system_vars, file_con)
    on.exit(close(file_con), add = TRUE)
    Sys.unsetenv(env_name)
    return(TRUE)
}

#' @title Check if environment variable is set in `.Renviron`
#' @param env_name character name of the environment variable to check.
#' @param renviron character either 'global', 'local' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
check_env_var <- function(env_name, renviron = "global") {
    rr <- read_renv(renviron)
    on.exit(close(rr$con), add = TRUE)
    any(grepl(env_name, rr$lines))
}
