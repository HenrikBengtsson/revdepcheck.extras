#' Pre-install and updated reverse-dependency packages
#'
#' @param pkgs (character vector) Packages to be pre-installed.
#'
#' @param skip If TRUE, packages already in the binary crancache cache will
#' be skipped.  If FALSE, all packages will be pre-installed.
#'
#' @param temp_lib_path (character string) The folder where to
#' install packages during pre-installation.
#'
#' @return Nothing.
#'
#' @details
#' Reverse-dependency packages are pre-installed to custom package
#' library folders with suffix \file{-revdepcheck} that lives next
#' to your default library folders.
#'
#' @importFrom parallelly availableCores
#' @importFrom future.apply future_lapply
#' @importFrom progressr progressor
#' @importFrom crancache install_packages
#' @export
revdep_preinstall <- function(pkgs, skip = TRUE, temp_lib_path = revdep_preinstall_libs()[1]) {
  oopts <- options(Ncpus = availableCores())
  
  pkgs <- unique(pkgs)
  message(sprintf("Triggering crancache builds by pre-installing %d packages: %s", length(pkgs), paste(sQuote(pkgs), collapse = ", ")))
  message(".libPaths():")
  message(paste(paste0(" - ", .libPaths()), collapse = "\n"))

  assert_repos()

  if (skip) {
    pkgs <- pkgs[!is_in_crancache(pkgs)]
    message(sprintf("After skipping already cached packages, pre-installing %d packages: %s", length(pkgs), paste(sQuote(pkgs), collapse = ", ")))
  }

  message(sprintf("Pre-installing %d packages (Ncpus = %d)",
                  length(pkgs), getOption("Ncpus", 1L)))

  message("Installing into library: ", sQuote(temp_lib_path))

  ## Install one-by-one to update cache sooner
  p <- progressor(along = pkgs)
  void <- lapply(pkgs, FUN = function(pkg) {
    on.exit(p(pkg))
    message(sprintf("Pre-installing package %s (Ncpus = %d)", pkg, getOption("Ncpus", 1L)))
    install_packages(pkg, dependencies = TRUE, lib = temp_lib_path)
  }) #, future.chunk.size = 1L, future.seed = TRUE)
  invisible(void)  
}

#' @rdname revdep_preinstall
#' @importFrom parallelly availableCores
#' @importFrom crancache update_packages
#' @export
revdep_preinstall_update <- function() {
  oopts <- options(Ncpus = availableCores())
  lib_paths_org <- .libPaths()
  on.exit({
    .libPaths(lib_paths_org)
    options(oopts)
  })
  .libPaths(revdep_preinstall_libs())
  
  message("Update crancache for all pre-installed packages:")
  message(".libPaths():")
  message(paste(paste0(" - ", .libPaths()), collapse = "\n"))
  message(sprintf("Ncpus=%d", getOption("Ncpus", 1L)))
  update_packages(ask = FALSE)
}

revdep_preinstall_libs <- function() {
  lib_paths <- .libPaths()
  lib_paths[1] <- sprintf("%s-revdepcheck", lib_paths[1])
  dir.create(lib_paths[1], recursive = TRUE, showWarnings = FALSE)
  lib_paths
}

