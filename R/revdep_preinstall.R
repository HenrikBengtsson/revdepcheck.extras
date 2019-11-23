#' @importFrom crancache install_packages
#' @export
revdep_preinstall <- function(pkgs) {
  pkgs <- unique(pkgs)
  lib_paths_org <- lib_paths <- .libPaths()
  on.exit(.libPaths(lib_paths_org))
  lib_paths[1] <- sprintf("%s-revdepcheck", lib_paths[1])
  dir.create(lib_paths[1], recursive = TRUE, showWarnings = FALSE)
  .libPaths(lib_paths)
  message(sprintf("Triggering crancache builds by pre-installing %d packages: %s", length(pkgs), paste(sQuote(pkgs), collapse = ", ")))
  message(".libPaths():")
  message(paste(paste0(" - ", .libPaths()), collapse = "\n"))
  ## Install one-by-one to update cache sooner
  for (kk in seq_along(pkgs)) {
    pkg <- pkgs[kk]
    message(sprintf("Pre-installing package %d of %d: %s",
                    kk, length(pkgs), pkg))
    install_packages(pkg)
  }
}
