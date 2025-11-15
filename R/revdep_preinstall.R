#' Pre-install and updated reverse-dependency packages
#'
#' @param pkgs (character vector) Packages to be pre-installed.
#'
#' @param skip If TRUE, packages already in the binary crancache cache will
#' be skipped.  If FALSE, all packages will be pre-installed.
#'
#' @param chunk_size If `1`, packages are installed one by one.
#' If `2`, packages are installed two by two, and so on.
#' If `Inf`, all packages are installed in one go.
#' This affects how often the **crancache** package library cache is
#' updated. The smaller the chunk size is the more frequently it is
#' updated.
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
#' Packages are forced to be installed from source
#' (https://github.com/r-lib/crancache/issues/46), regardless of
#' binary packages already existing in the cache or not.
#'
#' @importFrom parallelly availableCores
#' @importFrom future.apply future_lapply
#' @importFrom progressr progressor
#' @importFrom crancache install_packages
#' @importFrom parallel splitIndices
#' @export
revdep_preinstall <- function(pkgs, skip = TRUE, chunk_size = availableCores(), temp_lib_path = revdep_preinstall_libs()[1]) {
  oenv <- Sys.getenv("_R_CHECK_LIMIT_CORES_", NA_character_)
  if (!is.na(oenv)) {
    Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    on.exit(Sys.setenv("_R_CHECK_LIMIT_CORES_" = oenv))
  }
  
  oopts <- options(Ncpus = chunk_size)
  on.exit(options(oopts), add = TRUE)
  
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

  nbr_of_chunks <- ceiling(length(pkgs) / chunk_size)
  chunks <- splitIndices(length(pkgs), ncl = nbr_of_chunks)
  message(sprintf("Installing packages in %d chunks of %d packages each", length(chunks), chunk_size))
  
  p <- progressor(along = chunks)
  for (kk in seq_along(chunks)) local({
    pkgs_chunk <- pkgs[chunks[[kk]]]
    info <- sprintf("Pre-installing %d packages (%s)", length(pkgs_chunk), paste(sQuote(pkgs_chunk), collapse = ", "))
    message(sprintf("%d/%d. %s (Ncpus = %d)", kk, length(chunks), info, getOption("Ncpus", 1L)))
    p(info, amount = 0)
    on.exit(p())
    install_packages(pkgs_chunk, dependencies = TRUE, lib = temp_lib_path, type = "source")
  })
  
  invisible()  
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

