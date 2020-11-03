#' Precache package installs required for reverse-dependencies package checks
#'
#' @param package (character string) Name of package.
#'
#' @param temp_lib_path (character string) The folder where to
#' install packages during the pre-cache installation.  This
#' defaults to a temporary folder.
#'
#' @param \ldots Not used.
#'
#' @param dryrun (logical) If TRUE, then no packages are cached.
#'
#' @return (character vector) The packages that _failed_ to install.
#'
#' @details
#' This function populates the \pkg{crancache} package cache
#' that holds package source tarballs as well as package binaries.
#' This is done by calling [crancache::install_packages()] on
#' all reverse package dependencies as given by
#' [revdep_required_packages()].
#' These packages are installed to a temporary folder to avoid
#' adding them to your default package library folders.
#'
#' @importFrom future.apply future_lapply
#' @importFrom crancache crancache_list install_packages
#' @importFrom utils file_test
#'
#' @export
revdep_precache <- function(package, temp_lib_path = tempfile(pattern = "dir"), ..., dryrun = FALSE) {
  pkgs <- revdep_required_packages(package, ...)

  if (!file_test("-d", temp_lib_path)) {
    dir.create(temp_lib_path, recursive = TRUE)
    on.exit(unlink(temp_lib_path, recursive = TRUE))
  }

  ## Nothing todo?
  if (length(pkgs) == 0L) return(character(0L))

  ## Which packages are not in the cache already?
  cached <- unique(crancache_list()$Package)
  missing <- setdiff(pkgs, cached)
  ## Nothing todo?
  if (length(missing) == 0L) return(character(0L))
  
  message(sprintf("Pre-cache installing: [%d] %s", length(missing), paste(sQuote(missing), collapse = ", ")))

  if (dryrun) return(missing)

  future_lapply(missing, FUN = install_packages, lib = temp_lib_path, future.chunk.size = 1L, future.seed = TRUE)

  cached <- unique(crancache_list()$Package)
  missing <- setdiff(pkgs, cached)
  
  missing
}
