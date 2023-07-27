.onLoad <- function(lib, pkg) {
    op <- options()
    op_chewie <- list(
        chewie.print.class = TRUE,
        chewie.print.keys = TRUE,
        chewie.print.topn = 5L,
        chewie.print.nrows = 10L,
        chewie.print.trunc.cols = TRUE,
        chewie.prettyprint.char = 50L,
        chewie.print.width = 130L,
        chewie.session.cache = tempdir()
    )
    toset <- !(names(op_chewie) %in% names(op))
    if (any(toset)) options(op_chewie[toset])

    chewie_health_check()

    invisible()
}
