.onLoad <- function(lib, pkg) {
  op <- options()
  op_chewie <- list(
    # data.table options...
    chewie.print.topn = 5L,
    chewie.print.nrows = 10L,
    chewie.print.class = TRUE,
    chewie.print.rownames = TRUE,
    chewie.print.colnames = TRUE,
    chewie.print.keys = TRUE,
    chewie.print.trunc.cols = TRUE,
    chewie.prettyprint.char = 50L,
    # chewie options...
    chewie.print.width = 130L
  )
  toset <- !(names(op_chewie) %in% names(op))
  if (any(toset)) options(op_chewie[toset])

  if (is.na(chewie_get_cache())) {
    chewie_setup_cache(quiet = TRUE)
  }

  chewie_set_cache_opts()

  chewie_health_check(.test = FALSE, .report_cache = FALSE)

  invisible()
}
