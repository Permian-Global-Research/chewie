#' @title generic function for printing chewie objects
#' @param x chewie object to print
#' @noRd
chewie_print <- function(x, ...) {
    opts <- options(
        datatable.print.topn = getOption("chewie.print.topn"),
        datatable.print.nrows = getOption("chewie.print.nrows"),
        datatable.print.class = getOption("chewie.print.class"),
        data.table.print.row.names = getOption("chewie.print.rownames"),
        data.table.print.col.names = getOption("chewie.print.colnames"),
        datatable.print.keys = getOption("chewie.print.keys"),
        datatable.print.trunc.cols = getOption("chewie.print.trunc.cols"),
        datatable.prettyprint.char = getOption("chewie.prettyprint.char"),
        width = getOption("chewie.print.width")
    )

    on.exit(options(opts))

    cli::cli_h1(class(x)[1])
    cli::cli_inform(c("*" = paste0(
        "  ",
        chew_bold_green(attributes(x)$gedi_product)
    )))
    print(data.table::as.data.table(x), ...)
    cli::cli_h1("")
    invisible()
}
