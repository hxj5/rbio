# io.R

#' Force Flush Print
#'
#' @param s An object.
#' @return Void.
#' @seealso [print()]
#' @export
#' @examples
#' io_ffprint("hello world!")
io_ffprint <- function(s) {
  print(s)
  utils::flush.console()
}


#' Write TSV
#'
#' This function is aimed to write a dataframe into a TSV file in a 
#' commonly used format.
#' Specifically, it calls \code{\link[utils:write.table]{write.table()}} 
#' internally with 
#' parameters `sep = "\t"`, `quote = FALSE`, `row.names = FALSE`, and
#' `col.names = TRUE`.
#'
#' @param x A dataframe.
#' @param file A string. Path to the output TSV file.
#' @return Void.
#'
#' @seealso [write.table()]
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:5, y = 2:6)
#' io_write_tsv(df, "~/test_rrbio/io_write_tsv.tsv")}
io_write_tsv <- function(x, file) {
  utils::write.table(
    x = x,
    file = file,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
}


