#' @title Read the R environment file
#' @param renviron character either 'user', '"project"' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
read_renv <- function(renviron) {
  # Get the .Renviron on their system
  if (tolower(renviron) == "auto") {
    find_renv <- find_renviron()
    home <- ifelse(!is.null(find_renv),
      dirname(find_renv), Sys.getenv("HOME")
    )
    if (is.null(home)) {
      home <- Sys.getenv("HOME")
    }
  } else if (tolower(renviron) == "user") {
    home <- Sys.getenv("HOME")
  } else if (tolower(renviron) == "project") {
    home <- getwd()
  } else {
    if (dir.exists(renviron)) {
      home <- renviron
    } else {
      cli::cli_abort(".Renviron parent directory does not exist!")
    }
  }

  renv_path <- file.path(home, ".Renviron")

  if (!file.exists(renv_path)) {
    if (tolower(renviron) %in% c("auto", "user")) {
      inform_missing_user_renv()
      file.create(renv_path)
    } else {
      abort_missing_project_renv(home)
    }
  }

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



#' @title Add environment variable to `.Renviron`
#' @param env_name character name of the environment variable to add.
#' @param env_val character value of the environment variable to add.
#' @param renviron character either 'user', '"project"' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
add_env_var <- function(
    env_name, env_val,
    renviron) {
  if (isTRUE(check_env_var(env_name, renviron))) {
    if (!interactive()) {
      abort_env_set(env_name)
    }
    if (getOption("chewie.testing")) {
      remove_env_var(env_name, renviron)
      return(invisible())
    }

    inform_ask_env_overwrite(env_name)

    choice <- menu(c(
      chew_bold_green("Yes"),
      chew_bold_red("No, are you mad?!")
    ))

    switch(choice,
      remove_env_var(env_name, renviron),
      cli::cli_abort("Environment variable `{env_name}` not set.")
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
#' @param renviron character either 'user', '"project"' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
remove_env_var <- function(env_name, renviron) {
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
#' @param renviron character either 'user', '"project"' or path to the directory
#' containing the `.Renviron` file.
#' @noRd
check_env_var <- function(env_name, renviron) {
  rr <- read_renv(renviron)
  on.exit(close(rr$con), add = TRUE)
  any(grepl(env_name, rr$lines))
}

#' @title Create default chewie cache file location
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

#' @title Get NASA Earthdata Credentials environment
#' return character file path for `.netrc` file
#' @details `chewie_get_env` can be used to manually get the `CHEWIE_NETRC`
#' environment, providing the file path to the `.netrc` file.
#' @noRd
chewie_get_env <- function(.env = c("CHEWIE_NETRC", "CHEWIE_CACHE_HOME")) {
  .env <- rlang::arg_match(.env)
  Sys.getenv(.env, unset = NA)
}
