# matrix.R


#' DataFrame and Matrix Conversion
#'
<<<<<<< HEAD
#' The conversion between long-format dataframe and (sparse) matrix.
#'
#' @param df A long-format dataframe. It should contain at least 3 columns 
#'   specified by params `row`, `col`, and `value`.
#' @param mtx A matrix. It should be regular matrix for `mtx_mtx2df()` and
#'   sparse matrix of `dgCMatrix` class for `mtx_sparse_mtx2df()`.
=======
#' The conversion between dataframe and (sparse) matrix.
#'
#' @param df A dataframe. It should contain at least 3 columns specified by
#'   params `row`, `col`, and `value`.
>>>>>>> 42582cea2f43dac457911e8857992e2174bb4d6c
#' @param row A string. The name of a column in `df` that contains the row
#'   names of the matrix.
#' @param col A string. The name of a column in `df` that contains the column
#'   names of the matrix.
#' @param value A string. The name of a column in `df` that contains the
#'   values of the matrix.
#' @param na.values The value to replace NAs introduced during conversion.
#'   Setting to `NA` or `NULL` to skip the value replacement.
<<<<<<< HEAD
#' @param full A bool. Whether to return all combinations of rows and columns.
#' @return
#' * The `mtx_df2mtx()` returns a matrix.
#' * The `mtx_df2sparse_mtx()` returns a sparse matrix of `dgCMatrix` class.
#' * The `mtx_mtx2df()` returns a long-format dataframe.
#' * The `mtx_sparse_mtx2df()` returns a long-format dataframe.
=======
#' @return
#' * The `mtx_df2mtx` returns a matrix.
#' * The `mtx_df2sparse_mtx` returns a sparse matrix of `dgCMatrix` type.
>>>>>>> 42582cea2f43dac457911e8857992e2174bb4d6c
#'
#' @section Notes:
#' The param `na.values` could affect the `NA`s that already exist in the
#' dataframe or matrix before conversion, which could lead to an unexpected
#' result.
#'
#' @examples
#' df <- data.frame(
#'   x = c("x1", "x1", "x2"),
#'   y = c("y1", "y2", "y2"),
#'   v = 1:3,
#'   stringsAsFactors = FALSE
#' )
<<<<<<< HEAD
#' mtx1 <- mtx_df2mtx(df, row = "x", col = "y", value = "v")
#' mtx2 <- mtx_df2sparse_mtx(df, row = "x", col = "y", value = "v")
#' df1 <- mtx_mtx2df(mtx1, row = "x", col = "y", value = "v")
#' df2 <- mtx_sparse_mtx2df(mtx2, row = "x", col = "y", value = "v")
=======
#' mtx <- mtx_df2mtx(df, row = "x", col = "y", value = "v")
#' mtx2 <- mtx_df2sparse_mtx(df, row = "x", col = "y", value = "v")
>>>>>>> 42582cea2f43dac457911e8857992e2174bb4d6c
#' @name df-mtx-convert
NULL


#' @export
#' @rdname df-mtx-convert
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

<<<<<<< HEAD
  row_names <- df_tran[, 1]  
=======
  row_names <- df_tran[, 1]    # CHECK ME! what if duplicate column names?
>>>>>>> 42582cea2f43dac457911e8857992e2174bb4d6c
  df_tran <- df_tran[, -1]

  if (! is.na(na.values) && ! is.null(na.values))
    df_tran[is.na(df_tran)] <- na.values

  mtx <- as.matrix(df_tran)
  rownames(mtx) <- row_names
  return(mtx)
}


#' @export
#' @rdname df-mtx-convert
mtx_df2sparse_mtx <- function(df, row, col, value, na.values = 0) {
  mtx <- mtx_df2mtx(df, row = row, col = col, value = value, 
                    na.values = na.values)
<<<<<<< HEAD
  mtx_s <- methods::as(mtx, "dgCMatrix")
  return(mtx_s)
}


#' @export
#' @rdname df-mtx-convert
mtx_mtx2df <- function(mtx, row, col, value) {
  df <- as.data.frame(mtx)
  df <- cbind(rownames(df), df)
  rownames(df) <- NULL
  df_tran <- df %>%
    tidyr::gather(key = col, value = value, -1)
  colnames(df_tran) <- c(row, col, value)
  rownames(df_tran) <- 1:nrow(df_tran)
  return(df_tran)
}


#' @export
#' @rdname df-mtx-convert
mtx_sparse_mtx2df <- function(mtx, row, col, value, full = FALSE) {
  if (full) {
    mtx1 <- as.matrix(mtx)
    df <- mtx_mtx2df(mtx1, row = row, col = col, value = value)
    return(df)
  } else {
    # ref: https://stackoverflow.com/questions/53486135/converting-a-dgcmatrix-to-data-frame
    # and the `summary` method does not work.
    mtx1 <- methods::as(mtx, "dgTMatrix")

    df <- data.frame(
      row = rownames(mtx1)[mtx1@i + 1],
      col = colnames(mtx1)[mtx1@j + 1],
      value = mtx1@x,
      stringsAsFactors = FALSE
    )
    colnames(df) <- c(row, col, value)
    return(df)
  }
=======
  mtx_s <- Matrix::Matrix(mtx, sparse = TRUE)
  return(mtx_s)
>>>>>>> 42582cea2f43dac457911e8857992e2174bb4d6c
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

