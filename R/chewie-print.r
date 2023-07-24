#' @title generic function for printing chewie objects
#' @param x chewie object to print
#' @noRd
chewie_print <- function(x) {
    opts <- options(
        datatable.print.class = getOption("chewie.print.class"),
        datatable.print.keys = getOption("chewie.print.keys"),
        datatable.print.topn = getOption("chewie.print.topn"),
        datatable.print.trunc.cols = getOption("chewie.print.trunc.cols"),
        datatable.prettyprint.char = getOption("chewie.prettyprint.char"),
        width = getOption("chewie.print.width")
    )

    on.exit(options(opts))

    cli::cli_h1(class(x)[1])
    print(data.table::as.data.table(x))
}
