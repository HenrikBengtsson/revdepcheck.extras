#' Find all required reverse package dependencies
revdep_required_packages <- function(package, ...) {
  rdpkgs <- revdepcheck:::cran_revdeps(package)
  pkgs <- required_packages(rdpkgs, ...)
  pkgs <- unlist(pkgs, use.names = FALSE)
  pkgs <- sort(unique(c(rdpkgs, pkgs)))
  pkgs
}
