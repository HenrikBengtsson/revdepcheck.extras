#' Find all required reverse package dependencies
#'
#' @param package (character string) Name of package
#'
#' @param \ldots Not used.
#'
#' @export
revdep_required_packages <- function(package, ...) {
  cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
  rdpkgs <- cran_revdeps(package)
  pkgs <- required_packages(rdpkgs, ...)
  pkgs <- unlist(pkgs, use.names = FALSE)
  pkgs <- sort(unique(c(rdpkgs, pkgs)))
  pkgs
}
