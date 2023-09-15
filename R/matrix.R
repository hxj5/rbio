# matrix.R

# require
# - dplyr
# - tidyr
# - Matrix

mtx_df2mtx <- function(df, row, col, value, na.values = 0) {
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

  row_names <- df_tran[, 1]$row    # CHECK ME! what if duplicate column names?
  df_tran[, 1] <- NULL

  if (! is.na(na.values) && ! is.null(na.values))
    df_tran[is.na(df_tran)] <- na.values

  mtx <- as.matrix(df_tran)
  rownames(mtx) <- row_names
  return(mtx)
}


mtx_df2sparse_mtx <- function(df, row, col, value, na.values = 0) {
  func <- "mtx_df2sparse_mtx"

  mtx <- mtx_df2mtx(df, row = row, col = col, value = value)
  s_mtx <- as(mtx, "dgCMatrix")
  return(s_mtx)
}

