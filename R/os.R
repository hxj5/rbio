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

