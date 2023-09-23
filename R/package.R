# package.R - package management routine


#' @keywords internal
#' @noRd
pkg_install_core <- function(pkgs, args, lib,
                             update = FALSE, ask = FALSE, verbose = TRUE)
{
  # remove the LOCKed folders in `lib`.
  for (folder in dir(lib)) {
    res <- base::grep("^00LOCK", folder)
    if (length(res) > 0) {
      folder_path <- os_join_path(lib, folder)
      base::unlink(folder_path, recursive = TRUE)
      base::message(sprintf("The locked folder '%s' has been removed.", 
                            folder_path))
    }
  }

  pkgs_uniq <- unique(pkgs)

  # install packges
  args_others <- list(pkgs = pkgs_uniq, lib = lib, 
                      update = update, ask = ask, verbose = verbose)
  args_full <- c(args_others, args)
  do.call(BiocManager::install, args_full)
}


#' Package Installation
#'
#' This function is a wrapper of BiocManager::install(), allowing several
#' rounds of installation in case of errors during installation. Note that
#' BiocManager::install() can also install CRAN and GitHub packages, in 
#' addition to Bioconductor packages.
#'
#' @inheritParams BiocManager::install
#' @inheritParams utils::install.packages
#' @param ... Additional arguments used by `BiocManager::install()` and 
#'   `utils::install.packages()`.
#' @param max_try An integer. The maximum rounds of installation.
#' @return A vector of names of packages whose installation failed.
#'
#' @section Notes:
#' This wrapper function changes the default behavior of several parameters:
#' * The `lib`: default is `NULL`, which will use the first element of 
#'   [.libPaths()].
#' * The `update`: default is `FALSE`, which will not update the old packages.
#' * The `ask`: default is `FALSE`, which will not prompt user before 
#'   installed packages are updated. 
#'
#' @examples
#' \dontrun{
#' # install CRAN and Bioconductor packages
#' pkgs <- c("dplyr", "GenomicRanges")
#' pkg_install(pkgs)
#' }
#' @export
pkg_install <- function(pkgs, ..., lib = NULL,
                        update = FALSE, ask = FALSE, verbose = TRUE,
                        max_try = 3)
{
  if (is.null(lib)) {
    libs <- base::.libPaths()
    if (length(libs) <= 0)
      stop("libPaths is empty!")
    lib <- libs[1]
  }
  os_assert_e(lib)
  base::message(sprintf("lib path is '%s'.", lib))

  args <- list(...)

  pkgs_uniq <- unique(pkgs)
  pkgs_new <- pkgs_uniq

  for (n_try in 1:max_try) {
    is_installed <- sapply(pkgs_new, requireNamespace, quietly = TRUE)

    # check packages that have been installed
    pkgs_old <- pkgs_new[is_installed]
    if (length(pkgs_old) > 0) {
      base::message(
        sprintf("[ins-%d] %d packages have already been installed.",
                n_try, length(pkgs_old)))
      if (verbose)
        base::message(
          sprintf("[ins-%d] these packages are: %s.", 
                  n_try, paste(pkgs_old, collapse = ", ")))
    }

    # check packages that have not been installed
    pkgs_new <- pkgs_new[! is_installed]
    if (length(pkgs_new) > 0) {
      base::message(
        sprintf("[ins-%d] %d packages are to be installed.", 
                n_try, length(pkgs_new)))
      if (verbose)
        base::message(
          sprintf("[ins-%d] these packages are: %s.", 
                  n_try, paste(pkgs_new, collapse = ", ")))
    } else {
      base::message(
        sprintf("[ins-%d] all packages have been installed.", n_try))
      break
    }
  
    # install packages
    if (verbose)
      base::message(sprintf("[ins-%d] start to install ...", n_try))

    pkg_install_core(pkgs = pkgs_new, args = args, lib = lib,
                     update = update, ask = ask, verbose = verbose)
  }

  n_new <- length(pkgs_new)
  base::message(
    sprintf("summary: in total %d packages succeeded; %d packages failed.",
            length(pkgs_uniq) - n_new, n_new))
  if (n_new > 0) {
    base::message("packages failed to be installed:")
    base::message(paste(pkgs_new, collapse = ", "))
  }

  return(pkgs_new)
}


#' Using custom functions to install R packages
#'
#' The packages that need to be installed are all written in pkgs
#' "0" use BiocManager install; "1" use mirror install; 
#' It is recommended to install in both modes simultaneously, 
#' and program will automatically skip packages that have already been installed.
#' 
#' @section Notes:
#' The function pkgs_in() below is written by @Yijun Liu on Sep, 2023.
#'
#' @examples
#' \dontrun{
#' pkgs<-c("AnnotationDbi","impute","GO.db","preprocessCore")
#' pkgs_in(pkgs,"0")
#' pkgs2<-c("AnnotationDbi","impute","GO.db","preprocessCore")
#' pkgs_in(pkgs2,"1")
#' }
#' 
#' @keywords internal
#' @noRd
pkgs_in<-function(pkgs,mode){
##Title:TODO
##Objective:TODO
##Createdby:awa123
##Createdon:2023/9/22
#  #check current CRAN and Bioconductor
#  options()$repos
#  options()$BioC_mirror
#  BiocManager::repositories()
#  #Directly setting the mirror
#  options("repos"=c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
#  options("BioC_mirror"="http://mirrors.tuna.tsinghua.edu.cn/bioconductor/")
#  
#  if(!requireNamespace("BiocManager",quietly=TRUE)){
#    install.packages("BiocManager",ask=F,update=F)
#  }# check BiocManager
#  
#  if(!requireNamespace("stringr",quietly=TRUE)){
#    install.packages("stringr",ask=F,update=F)
#  }# check stringr
#  
#  #Remove duplicates and identify if there are installation packages in Github format
#  pkgs<-unique(pkgs)
#  pkgs2<-pkgs
#  logi<-stringr::str_detect(pkgs2,"/")
#  pkgs2[logi]<-stringr::str_match(pkgs2[logi],".*/(.*)$")[,2]
#  
#  #Install packages that have not yet been installed in pkgs
#  new<-!(sapply(pkgs2,requireNamespace,quietly=T))
#  
#  #Show packages to be installed
#  if(sum(new)>0){
#    cat("pkgstoinstall:",pkgs[new],"\n")
#  }else{
#    cat("All pkgs already installed \n")
#  }
#  
#  #installpkgs
#  if(mode=="0")
#  {if(any(new))BiocManager::install(pkgs[new],ask=F,update=F)}
#  if(mode=="1")
#  {
#    site="https://mirrors.tuna.tsinghua.edu.cn/CRAN"
#    if(any(new))install.packages(pkgs[new],repos=site)
#  }
}

