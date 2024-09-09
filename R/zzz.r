.onLoad <- function(lib, pkg) {
  op <- options()

  # TODO: consider the ordering of these codecs which is preferred for chewie?
  arrow_comp_avail <- sapply(
    c("brotli", "zstd", "gzip", "snappy", "bz2", "lz4", "lzo", "uncompressed"),
    arrow::codec_is_available
  )

  # select first true value in arrow_comp_avail
  arrow_comp_pref <- names(arrow_comp_avail)[arrow_comp_avail][1]

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
    chewie.print.width = 130L,
    chewie.parquet.codec = arrow_comp_pref
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
