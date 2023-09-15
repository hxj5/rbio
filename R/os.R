# os.R

os_assert_e <- function(path, type = "file") {
  func <- "os_assert_e"

  if (type == "file")
    if (! file.exists(path))
      stop(sprintf("[E::%s] '%s' does not exist!", func, path))
  else if (type == "dir")
    if (! dir.exists(path))
      stop(sprintf("[E::%s] '%s' does not exist!", func, path))
  else
      stop(sprintf("[E::%s] unknown type '%s'!", func, type))
}


os_join_path <- function(path1, path2) {
  if (0 == (n <- nchar(path1)) || 0 == nchar(path2)) 
    return(NULL)
  sep <- "/"
  if ('/' == substr(path1, n, n))
    sep <- ""
  return(paste0(path1, sep, path2))
}


os_safe_mkdir <- function(dir, recursive = TRUE) {
  if (! dir.exists(dir))
    dir.create(dir, recursive = recursive)
}


