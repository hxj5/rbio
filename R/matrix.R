# matrix.R

# require
# - dplyr
# - tidyr
# - Matrix

mtx_df2mtx <- function(df, row, col, value) {
  func <- "mtx_df2mtx"

  if (! row %in% colnames(df))
    stop(sprintf("[E::%s] column '%s' is not in dataframe.", func, row))
  if (! col %in% colnames(df))
    stop(sprintf("[E::%s] column '%s' is not in dataframe.", func, col))
  if (! value %in% colnames(df))
    stop(sprintf("[E::%s] column '%s' is not in dataframe.", func, value))

  df <- df[, c(row, col, value)]
  colnames(df) <- c("row", "col", "value")

  df_tran <- df %>%
    tidyr::spread(key = col, value = value)

  mtx <- as.matrix(df_tran)
  return(mtx)
}


mtx_df2sparse_mtx <- function(df, row, col, value) {
  func <- "mtx_df2sparse_mtx"

  mtx <- mtx_df2mtx(df, row = row, col = col, value = value)
  s_mtx <- as(mtx, "dgCMatrix")
  return(s_mtx)
}

