=======
History
=======

Release v0.0.3 (23/09/2023)
===========================
* add time.R, io.R, splice_count.R, and package.R.
* mtx_df2mtx(): fix bug that extracting column from tibble with column index 
  returns a one-column tibble.
* spmtx-io: add parameters row_index and col_index.
* spmtx-io: change returned values to a list of elements instead of single
  sparse matrix.
* spmtx-io: add sanity check of input matrices.
* spmtx-io: add mtx_load_sparse_mtx_n() and mtx_save_sparse_mtx_n() for 
  processing a list of sparse matrices.
* matrix.R: wrap dgCMatrix conversion.
* documenting: improve links in function documenting.
* documenting: dontrun (do not run) examples that add files or folders.

Release v0.0.2 (19/09/2023)
===========================
* add os.R and matrix.R (df-mtx-convert and spmtx-io).
* start to use devtools for package development.
* add Installation to README.
* add release history.

Release v0.0.1 (15/09/2023)
===========================
* initialize the project.

