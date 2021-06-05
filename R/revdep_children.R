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
      pkgs <- unique(pkgs)
      cache[[pkg]] <- pkgs
    }
    pkgs
  }
})

#' @param exclude_children If TRUE, only second generation dependencies are
#' returned, excluding first generation dependencies ("children").  If FALSE,
#' all reverse dependencies are returned regardless of generation.
#'
#' @rdname revdep_children
#' @importFrom progressr progressor
#' @importFrom future.apply future_lapply
#' @export
revdep_grandchildren <- local({
  cache <- list()
  function(pkg = NULL, exclude_children = TRUE) {
    if (is.null(pkg)) pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cache[[pkg]]
    if (is.null(pkgs)) {
      children <- revdep_children(pkg)
      p <- progressor(along = children)
      pkgs <- future_lapply(children, FUN = function(child) {
        deps <- cran_revdeps(child)
        pkgs <- c(pkgs, deps)
        p(sprintf("%s (n=%d)", child, length(deps)))
      })
      pkgs <- unlist(pkgs, use.names = FALSE)
      pkgs <- unique(pkgs)
      if (exclude_children) pkgs <- setdiff(pkgs, children)
      cache[[pkg]] <- pkgs
    }
    pkgs
  }
})


