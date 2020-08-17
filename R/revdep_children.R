#' Gets reverse-dependency packages of a package
#'
#' @param pkg (character) The package name whose reverse dependencies should
#' be identified and returned.  If NULL (default), then the current package
#' according to [revdep_this_package()] is used.
#'
#' @return (character vector) List of package names
#'
#' Internally, memoization is used such that all succeeding
#' look-ups in the same R session for the same package will
#' return immediately.
#'
#' @export
revdep_children <- local({
  cache <- list()
  function(pkg = NULL) {
    if (is.null(pkg)) pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cache[[pkg]]
    if (is.null(pkgs)) {
      pkgs <- cran_revdeps(pkg)
      pkgs <- setdiff(pkgs, pkg) ## WORKAROUND
      cache[[pkg]] <- pkgs
    }
    pkgs
  }
})
