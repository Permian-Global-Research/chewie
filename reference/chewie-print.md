# print a `chewie.find` object

`print.chewie.find` is a method for printing `chewie.find` objects.
Essentially a wrapper for `print.data.table` with some custom options.
See details for options.

## Usage

``` r
# S3 method for class 'chewie.find'
print(x, ...)
```

## Arguments

- x:

  chewie.find object to print

- ...:

  arguments passed to
  [`data.table::print.data.table`](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Details

Edit the following options to change the default printing behaviour of
`chewie.find` objects: `chewie.print.class`, `chewie.print.keys`,
`chewie.print.topn`, `chewie.print.nrows`, `chewie.print.trunc.cols`,
`chewie.prettyprint.char`, `chewie.print.width`. Additonal options for
`print.data.table` can also be set to alter the print layout.
