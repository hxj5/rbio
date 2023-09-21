# matrix.R - (sparse) matrix routine


#' DataFrame and Matrix Conversion
#'
#' The conversion between long-format dataframe and (sparse) matrix.
#'
#' @param df A long-format dataframe. It should contain at least 3 columns 
#'   specified by params `row`, `col`, and `value`.
#' @param mtx A matrix. It should be regular matrix for `mtx_mtx2df()` and
#'   sparse matrix of `dgCMatrix` class for `mtx_sparse_mtx2df()`.
#' @param row A string. The name of a column in `df` that contains the row
#'   names of the matrix.
#' @param col A string. The name of a column in `df` that contains the column
#'   names of the matrix.
#' @param value A string. The name of a column in `df` that contains the
#'   values of the matrix.
#' @param na.values The value to replace `NA`s introduced during conversion.
#'   Setting to `NA` or `NULL` to skip the value replacement.
#' @param full A bool. Whether to return all combinations of rows and columns.
#' @return
#' * The `mtx_df2mtx()` returns a matrix.
#' * The `mtx_df2sparse_mtx()` returns a sparse matrix of `dgCMatrix` class.
#' * The `mtx_mtx2df()` returns a long-format dataframe.
#' * The `mtx_sparse_mtx2df()` returns a long-format dataframe.
#'
#' @section Notes:
#' The param `na.values` could affect the `NA`s that already exist in the
#' dataframe or matrix before conversion, which could lead to an unexpected
#' result.
#'
#' @seealso \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} and 
#'   \code{\link[Matrix:dgTMatrix-class]{dgTMatrix}}
#'
#' @examples
#' df <- data.frame(
#'   x = c("x1", "x1", "x2"),
#'   y = c("y1", "y2", "y2"),
#'   v = 1:3,
#'   stringsAsFactors = FALSE
#' )
#' mtx1 <- mtx_df2mtx(df, row = "x", col = "y", value = "v")
#' mtx2 <- mtx_df2sparse_mtx(df, row = "x", col = "y", value = "v")
#' df1 <- mtx_mtx2df(mtx1, row = "x", col = "y", value = "v")
#' df2 <- mtx_sparse_mtx2df(mtx2, row = "x", col = "y", value = "v")
#'
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
  colnames(df) <- c("df_row", "df_col", "df_value")

  df_tran <- df %>%
    tidyr::spread(key = "df_col", value = "df_value")

  row_names <- df_tran[, 1]  
  if (! is.null(dim(row_names))) {   # not a vector
    row_names <- row_names$df_row
  }
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
}


#' Sparse Matrix Input/Output
#'
#' These functions are aimed to load and save the sparse matrices in 
#' Matrix Market Format, together with its annotation files.
#'
#' @param mtx A matrix. It could be either sparse matrix or regular matrix.
#' @param in_dir A string. Path to the input directory.
#' @param out_dir A string. Path to the output directory.
#' @param mtx_fn A string. Path to the file storing the sparse matrix, in
#'   Matrix Market Format.
#' @param row_fn A string. Path to the file storing the annotation of matrix
#'   rows. Setting to NULL if there is no such annotation.
#' @param col_fn A string. Path to the file storing the annotation of matrix
#'   columns. Setting to NULL if there is no such annotation.
#' @param row_header A bool. Whether `row_fn` has header.
#' @param col_header A bool. Whether `col_fn` has header.
#' @param row_index An integer or a string. The index (integer) or name
#'   (string) of the column in `row_fn` that stores the matrix row annotation.
#' @param col_index An integer or a string. The index (integer) or name
#'   (string) of the column in `col_fn` that stores the matrix column 
#'   annotation.
#' @return
#' * The `mtx_load_sparse_mtx()` returns a sparse matrix of `dgCMatrix` class.
#' * The `mtx_save_sparse_mtx()` returns Void.
#' 
#' @section Notes:
#' The matrix file, row annotation file (if available) and column annotation
#' file (if available) should be in the same directory. 
#'
#' @seealso \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} and 
#'   \code{\link[Matrix:dgTMatrix-class]{dgTMatrix}}
#'
#' @examples
#' m <- matrix(rpois(20, 1), nrow = 5)
#' rownames(m) <- paste0("x", 1:5)
#' colnames(m) <- paste0("y", 1:4)
#' \dontrun{
#' mtx_save_sparse_mtx(m, "~/test_rrbio", "matrix.mtx", "rows.tsv", "cols.tsv")
#' m2 <- mtx_load_sparse_mtx("~/test_rrbio", "matrix.mtx", "rows.tsv", "cols.tsv")}
#'
#' @name spmtx-io
NULL


#' @export
#' @rdname spmtx-io
mtx_load_sparse_mtx <- function(in_dir, mtx_fn, row_fn = NULL, col_fn = NULL,
                                row_header = FALSE, col_header = FALSE,
                                row_index = 1, col_index = 1) {
  os_assert_e(in_dir)

  mtx_fpath <- os_join_path(in_dir, mtx_fn)
  os_assert_e(mtx_fpath)
  mtx <- Matrix::readMM(mtx_fpath)

  if (! is.null(row_fn)) {
    row_fpath <- os_join_path(in_dir, row_fn)
    os_assert_e(row_fpath)
    row_anno <- read.delim(row_fpath, header = row_header, 
                           stringsAsFactors = FALSE)
    row_names <- row_anno[, row_index]  # it is safe for dataframe, but not for tibble.
    rownames(mtx) <- row_names
  }

  if (! is.null(col_fn)) {
    col_fpath <- os_join_path(in_dir, col_fn)
    os_assert_e(col_fpath)
    col_anno <- read.delim(col_fpath, header = col_header, 
                           stringsAsFactors = FALSE)
    col_names <- col_anno[, col_index]
    colnames(mtx) <- col_names
  }

  mtx <- methods::as(mtx, "dgCMatrix")
  return(mtx)
}


#' @export
#' @rdname spmtx-io
mtx_save_sparse_mtx <- function(mtx, out_dir, mtx_fn, row_fn = NULL, col_fn = NULL) {
  os_safe_mkdir(out_dir, recursive = TRUE)

  if (! any(class(mtx) %in% c("dgCMatrix", "dgTMatrix"))) {
    mtx <- methods::as(mtx, "dgTMatrix")
  }

  mtx_fpath <- os_join_path(out_dir, mtx_fn)
  Matrix::writeMM(mtx, mtx_fpath)

  if (! is.null(row_fn)) {
    row_fpath <- os_join_path(out_dir, row_fn)
    write(rownames(mtx), row_fpath, sep = "\n")
  }

  if (! is.null(col_fn)) {
    col_fpath <- os_join_path(out_dir, col_fn)
    write(colnames(mtx), col_fpath, sep = "\n")
  }
}

