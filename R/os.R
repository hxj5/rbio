# os.R

#' Assert Existencce of File or Directory
#'
#' This function is aimed to assert the existence of specified file or 
#' directory.
#'
#' @param path A string. Path to a file or directory.
#' @param type A string. The path type, "file" or "dir".
#' @return Void.
#'
#' @section Details:
#' It will call \code{\link[base]{stop}} and raise an error if the path does not exist. 
#'
#' @export
#' @examples
#' \dontrun{os_assert_e("~/test")
#' os_assert_e("~/debug/test.R")}
os_assert_e <- function(path, type = "file") {
  if (type == "file")
    if (! file.exists(path))
      stop(sprintf("'%s' does not exist!", path))
  else if (type == "dir")
    if (! dir.exists(path))
      stop(sprintf("'%s' does not exist!", path))
  else
      stop(sprintf("unknown type '%s'!", type))
}


#' Join Two Path
#'
#' This function is aimed to concatenate two path. It should be commonly
#' used to concatenate a directory and a file path to generate the absolute
#' path of the file.
#'
#' @param path1 A string. The first path.
#' @param path2 A string. The second path.
#' @return A string. Concatenated path.
#'
#' @export
#' @examples
#' dir_path <- "~/debug"
#' file_path <- "test.R"
#' full_path <- os_join_path(dir_path, file_path)  # "~/debug/test.R"
os_join_path <- function(path1, path2) {
  if (0 == (n <- nchar(path1)) || 0 == nchar(path2)) 
    return(NULL)
  sep <- "/"
  if ('/' == substr(path1, n, n))
    sep <- ""
  return(paste0(path1, sep, path2))
}


#' Create Directory
#'
#' This function is aimed to create a directory if it does not exist.
#'
#' @param dir A string. Path to the directory.
#' @param recursive A bool. Whether to create the sub-directory recursively.
#' @return Void.
#'
#' @export
#' @examples
#' os_safe_mkdir("~/debug/test1/test2")
os_safe_mkdir <- function(dir, recursive = TRUE) {
  if (! dir.exists(dir))
    dir.create(dir, recursive = recursive)
}

