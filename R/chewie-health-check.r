#' @title Check the status of the {chewie}
#' @description checks the NASA Earthdata Credentials and GEDI Cache
#' environment variables
#' @export
chewie_health_check <- function() {
    if (is.na(chewie_get_env())) {
        inform_env_health("No NASA Earthdata Credentials set.")
    } else {
        if (!file.exists(chewie_get_env())) {
            inform_env_health("NASA Earthdata Credentials file does not exist.")
        } else {
            cli::cli_alert_success(c(
                "NASA Earthdata Credentials already set."
            ))
        }
    }

    if (is.na(chewie_get_cache())) {
        inform_cache_health("No GEDI Cache set.")
    } else {
        if (!dir.exists(chewie_get_cache())) {
            inform_cache_health("GEDI Cache directory does not exist.")
        } else {
            inform_cache_set_success(chewie_get_cache())
        }
    }
}
