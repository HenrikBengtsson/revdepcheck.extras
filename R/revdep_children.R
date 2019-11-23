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
