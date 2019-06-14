#' @importFrom future.apply future_lapply
#' @importFrom crancache crancache_list install_packages
#' @importFrom utils file_test
#' @export
revdep_precache <- function(package, temp_lib_path = tempfile(pattern = "dir"), ..., dryrun=FALSE) {
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

  future_lapply(missing, FUN = install_packages, lib = temp_lib_path, future.chunk.size = 1L)

  cached <- unique(crancache_list()$Package)
  setdiff(pkgs, cached)
}
