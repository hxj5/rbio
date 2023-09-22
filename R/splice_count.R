# splice_count.R - spliced/unspliced UMI/read counting


#' Spliced and Unspliced Count Matrix Input/Output
#'
#' These functions are aimed to read and write spliced and unspliced UMI/read
#' count matrices generated from scRNA-seq data.
#'
#' @param in_dir Path to the input directory.
#' @param out_dir A string. Path to the output directory.
#' @param spliced_mtx The `gene x cell` spliced count matrix.
#' @param spliced_fn A string. Path to the spliced matrix file.
#' @param unspliced_mtx The `gene x cell` unspliced count matrix.
#' @param unspliced_fn A string. Path to the unspliced matrix file.
#' @param ambiguous_mtx The `gene x cell` ambiguous count matrix.
#' @param ambiguous_fn A string. Path to the ambiguous matrix file.
#' @param gene_fn A string. Path to the gene annotation file.
#' @param cell_fn A string. Path to the cell annotation file.
#' @param gene_header A bool. Whether `gene_fn` has header.
#' @param cell_header A bool. Whether `cell_fn` has header.
#' @param gene_index An integer or a string. The index (integer) or name
#'   (string) of the column in `gene_fn` that stores the annotation.
#' @param cell_index An integer or a string. The index (integer) or name
#'   (string) of the column in `cell_fn` that stores the annotation.
#' @return 
#' * The `spcnt_load_matrices()` returns a list of five elements: (1) `spliced`:
#'   the spliced count matrix; (2) `unspliced`: the unspliced count matrix; (3)
#'   `ambiguous`: the ambiguous count matrix; (4) `gene_anno`: a dataframe, the 
#'   annotation of genes; (5) `cell_anno`: a dataframe, the annotations of 
#'   cells. Note that `spliced`, `unspliced`, and `ambiguous` are all of 
#'   `dgCMatrix` class.
#' * The `spcnt_save_matrices()` returns Void.
#'
#' @seealso [mtx_load_sparse_mtx_n()] and [mtx_save_sparse_mtx_n()]
#'
#' @examples
#' \dontrun{
#' genes <- paste0("gene", 1:5)
#' cells <- paste0("cell", 1:4)
#'
#' spliced_mtx <- matrix(rpois(20, 1), nrow = 5)
#' rownames(spliced_mtx) <- genes
#' colnames(spliced_mtx) <- cells
#'
#' unspliced_mtx <- matrix(rpois(20, 1), nrow = 5)
#' rownames(unspliced_mtx) <- genes
#' colnames(unspliced_mtx) <- cells
#'
#' ambiguous_mtx <- matrix(0, nrow = 5, ncol = 4)
#' rownames(ambiguous_mtx) <- genes
#' colnames(ambiguous_mtx) <- cells
#'
#' spcnt_save_matrices(
#'   "~/test_rrbio", 
#'   spliced_mtx, "spliced.mtx",
#'   unspliced_mtx, "unspliced.mtx",
#'   ambiguous_mtx, "ambiguous.mtx",
#'   "genes.tsv", "barcodes.tsv"
#' )
#'
#' res <- spcnt_load_matrices(
#'   "~/test_rrbio", "spliced.mtx", "unspliced.mtx", "ambiguous.mtx",
#'   "genes.tsv", "barcodes.tsv"
#' )
#' }
#'
#' @name spcnt-mtx-io
NULL


#' @export
#' @rdname spcnt-mtx-io
spcnt_load_matrices <- function(in_dir, spliced_fn, unspliced_fn, 
                                ambiguous_fn = NULL,
                                gene_fn = NULL, cell_fn = NULL,
                                gene_header = FALSE, cell_header = FALSE,
                                gene_index = 1, cell_index = 1)
{
  mtx_fn_list <- list(
    spliced = spliced_fn,
    unspliced = unspliced_fn,
    ambiguous = ambiguous_fn
  )

  res_raw <- mtx_load_sparse_mtx_n(
    in_dir = in_dir, mtx_fn_list = mtx_fn_list,
    row_fn = gene_fn, col_fn = cell_fn,
    row_header = gene_header, col_header = cell_header,
    row_index = gene_index, col_index = cell_index
  )

  res <- list(
    spliced_mtx = res_raw[["mtx"]][["spliced"]],
    unspliced_mtx = res_raw[["mtx"]][["unspliced"]],
    ambiguous_mtx = res_raw[["mtx"]][["ambiguous"]],
    gene_anno = res_raw[["row_anno"]],
    cell_anno = res_raw[["col_anno"]]
  ) 

  return(res)
}


#' @export
#' @rdname spcnt-mtx-io
spcnt_save_matrices <- function(out_dir, 
                                spliced_mtx, spliced_fn,
                                unspliced_mtx, unspliced_fn,
                                ambiguous_mtx = NULL, ambiguous_fn = NULL,
                                gene_fn = NULL, cell_fn = NULL) 
{
  mtx_list <- list(
    spliced = spliced_mtx,
    unspliced = unspliced_mtx,
    ambiguous = ambiguous_mtx
  )

  mtx_fn_list <- list(
    spliced = spliced_fn,
    unspliced = unspliced_fn,
    ambiguous = ambiguous_fn
  )

  mtx_save_sparse_mtx_n(
    mtx_list = mtx_list, 
    out_dir = out_dir,
    mtx_fn_list = mtx_fn_list,
    row_fn = gene_fn,
    col_fn = cell_fn
  )
}

