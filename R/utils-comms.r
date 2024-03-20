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
      chew_bold_cyan("2A"), ", ",
      chew_bold_cyan("2B"), ", and ",
      chew_bold_cyan("4A"), "."
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

abort_missing_lon_lat <- function(x) {
  if (find_gedi_product(x) == "1B") {
    lat_col <- "latitude_bin0"
    lon_col <- "longitude_bin0"
  } else {
    lat_col <- "lat_lowestmode"
    lon_col <- "lon_lowestmode"
  }
  cli::cli_abort(c(
    "x" = "No '{lat_col}' or '{lon_col}' columns found in gedi data.",
    "i" = "make sure you haven't removed these unintentionally using
            `dplyr::select` or similar.",
    "i" = "If you don't want a geometry column then you can just use
            `dplyr::collect` instead of `collect_gedi`."
  ))
}

abort_missing_project_renv <- function(x) {
  cli::cli_abort(c(".Renviron file does not exist in {x}",
    "i" = "Please create it first using (for example):",
    chew_bold_green("usethis::edit_r_environ(scope='project')")
  ))
}


abort_bool <- function(x) {
  arg <- names(x)
  cli::cli_abort(c(
    paste0(
      "The provided value for ",
      chew_bold_mag("`{arg}`"),
      " is not a boolean."
    ),
    "i" = "Please provide either TRUE or FALSE."
  ))
}

abort_download_with_log <- function(log_path) {
  cli::cli_abort(c(
    "x" = "Some Downloads have not completed successfully.",
    "i" = "Saving log file. To read the log, use:",
    ">" = paste0(
      chew_bold_cyan("readRDS("),
      chew_bold_yel(paste0('"', log_path, '"')),
      chew_bold_cyan(")")
    )
  ))
}

# ---- inform ----
#' @noRd
inform_reg_account <- function() {
  cli::cli_inform(
    c(
      paste0(
        chew_bold_green(cli::symbol$arrow_right),
        " Opening ", cli::style_bold("https://urs.earthdata.nasa.gov/users/new")
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
  ), class = "packageStartupMessage")
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
  ), class = "packageStartupMessage")
}

inform_time <- function(st, type) {
  tot_time <- Sys.time() - st
  time_units <- attr(tot_time, "units")
  num_time <- round(as.numeric(tot_time, units = time_units), 1)
  cli::cli_alert_info("{type} time: {num_time} {time_units}")
}

inform_download_completed <- function(ncomp, n) {
  cli::cli_alert_info(
    " {ncomp}/{n} {cli::qty(ncomp)}file{?s} were already downloaded."
  )
}

inform_missing_user_renv <- function() {
  cli::cli_inform(c(
    "!" = "No '.Renviron' file found in your home directory.",
    ">" = "chewie is creating one for you now..."
  ))
}

inform_find_gedi_cache <- function(fn, fs) {
  cli::cli_inform(
    c(
      "i" =
        paste0(
          "The ",
          chew_bold_yel("`find_gedi`"),
          " cache contains ",
          chew_bold_green("{fn} file{?s}"),
          " with a total size of ",
          chew_bold_mag("{fs} MB.")
        )
    ),
    class = "packageStartupMessage"
  )
}

inform_n_to_convert <- function(gedi_product, nfiles) {
  cli::cli_inform(c(">" = paste0(
    "Converting {nfiles} ",
    chew_bold_yel(gedi_product),
    " hdf files to parquet."
  )))
}

inform_n_to_download <- function(gedi_product, nfiles) {
  cli::cli_inform(c(">" = paste0(
    "Downloading {nfiles} ",
    chew_bold_yel(gedi_product),
    " Data files."
  )))
}

inform_no_select_cache <- function() {
  cli::cli_inform(
    c(
      "!" = "No parquet cache directories were selected.",
      "i" = "Please give one or more of the following as the `directories` argument:",
      paste0(
        cli::col_br_yellow("1B, 2A, 2B "),
        "and/or ",
        cli::col_br_yellow("4A")
      )
    )
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
      "i" = "Only objects of class `chewie.*` or created with `collect_gedi`
      are supported."
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

no_tx_waveform_warn <- function() {
  cli::cli_alert_warning(
    c(
      "The extraction of ",
      cli::style_bold("`txwaveform`"),
      " is not currently supported"
    )
  )
}


# ---- menus ----

cache_clear_check <- function(prod_name = c("1B", "2A", "2B", "4A", "H5")) {
  prod_name <- rlang::arg_match(prod_name, multiple = TRUE)
  get_cache_type <- function(cd) {
    unique(dplyr::case_match(
      cd,
      c("1B", "2A", "2B", "4A") ~ "parquet cache director",
      "H5" ~ "temp cache director"
    ))
  }

  cache_type <- get_cache_type(prod_name)

  cli::cli_inform(
    paste0(
      chew_bold_mag("?"),
      paste0(
        "   Do you really want to clear the GEDI {.pkg {prod_name}} ",
        cache_type, "{?y/ies}."
      )
    )
  )

  menu(c(
    chew_bold_green("Yes"),
    chew_bold_red("No!")
  ))
}
