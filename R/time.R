# time.R


#' @keywords internal
time_time <- function(time = NULL) {
  if (is.null(time))
    time <- Sys.time()
  else if (any(class(time) == "character"))
    time <- as.POSIXct(time)
  return(time)
}


#' Format Time String
#'
#' These functions are aimed to format time string.
#'
#' @param time A `POSIXct` object. Setting to `NULL` to use current time.
#' @param sep A string. Delimiter to join the sub-string in `format`.
#' @param format A string. A `format` string, e.g., "`%Y-%m-%d %H:%M:%S`".
#' @return Every function returns a string.
#'
#' @section Notes:
#' The `format` string for different functions:
#' * for `time_str_date()`: "`%Y`", "`%m`", "`%d`" joined by `sep`.
#' * for `time_str_time()`: "`%H`", "`%M`", "`%S`" joined by `sep`.
#' * for `time_str_datetime()`: "`%Y-%m-%d %H:%M:%S`".
#' * for `time_str_datetime0()`: "`%Y%m%d %H:%M:%S`".
#' * for `time_strftime()`: specified by user. Default is "`%Y-%m-%d %H:%M:%S`"
#'   when `format` is set to `NULL`.
#'
#' @seealso [POSIXct] and [base::format()]
#'
#' @examples
#' p <- as.POSIXct("2023-09-19 10:00:00")
#' time_str_date(p)       # "20230919"
#' time_str_date(p, sep = "-")  # "2023-09-19"
#' time_str_time(p)       # "10:00:00"
#'
#' # use current time
#' time_str_date()  
#' time_str_time()  
#' time_str_datetime()  
#' time_str_datetime0()
#'
#' @name time-str
NULL


#' @export
#' @rdname time-str
time_str_date <- function(time = NULL, sep = "") {
  fmt <- paste(c("%Y", "%m", "%d"), collapse = sep)
  s <- time_strftime(time, fmt)
  return(s)
}


#' @export
#' @rdname time-str
time_str_time <- function(time = NULL, sep = ":") {
  fmt <- paste(c("%H", "%M", "%S"), collapse = sep)
  s <- time_strftime(time, fmt)
  return(s)
}


#' @export
#' @rdname time-str
time_str_datetime <- function(time = NULL) {
  fmt <- "%Y-%m-%d %H:%M:%S"
  s <- time_strftime(time, fmt)
  return(s)
}


#' @export
#' @rdname time-str
time_str_datetime0 <- function(time = NULL) {
  fmt <- "%Y%m%d %H:%M:%S"
  s <- time_strftime(time, fmt)
  return(s)
}


#' @export
#' @rdname time-str
time_strftime <- function(time = NULL, format = NULL) {
  time <- time_time(time)
  if (is.null(format))
    format <- "%Y-%m-%d %H:%M:%S"
  s <- base::format(time, format)
  return(s)
}

