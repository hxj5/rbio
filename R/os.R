# os.R

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


