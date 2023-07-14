#' Check if package is installed, if not, abort with message.
#' @noRd
chk_pkg <- function(pkg, abort_fun) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        return(invisible())
    } else {
        abort_fun()
    }
}
