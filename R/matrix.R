# matrix.R

# require
# - dplyr
# - tidyr
# - Matrix

mtx_df2mtx <- function(df, row, col, value, na.values = 0) {
  if (! row %in% colnames(df))
    stop(sprintf("column '%s' is not in dataframe.", row))
  if (! col %in% colnames(df))
    stop(sprintf("column '%s' is not in dataframe.", col))
  if (! value %in% colnames(df))
    stop(sprintf("column '%s' is not in dataframe.", value))

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
  mtx <- mtx_df2mtx(df, row = row, col = col, value = value)
  s_mtx <- as(mtx, "dgCMatrix")
  return(s_mtx)
}


mtx_save_sparse_mtx <- function(mtx, out_dir, mtx_fn, row_fn = NULL, col_fn = NULL) {
  os_safe_mkdir(out_dir, recursive = TRUE)

  row_fpath <- os_join_path(out_dir, row_fn)
  col_fpath <- os_join_path(out_dir, col_fn)
  mtx_fpath <- os_join_path(out_dir, mtx_fn)

  write(rownames(mtx), row_fpath, sep = "\n")
  write(colnames(mtx), col_fpath, sep = "\n")
  Matrix::writeMM(mtx, mtx_fpath)
}

