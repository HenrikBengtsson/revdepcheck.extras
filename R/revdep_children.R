#' Gets reverse-dependency packages of a package
#'
#' @param package (character) A package name.
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
  function(package = ".") {
    if (identical(package, ".")) package <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cache[[package]]
    if (is.null(pkgs)) {
      pkgs <- cran_revdeps(package)
      pkgs <- setdiff(pkgs, package) ## WORKAROUND
      pkgs <- unique(pkgs)
      cache[[package]] <- pkgs
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
#' @export
revdep_grandchildren <- local({
  cache <- list()
  function(package = ".", exclude_children = TRUE) {
    if (identical(package, ".")) package <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cache[[package]]
    if (is.null(pkgs)) {
      children <- revdep_children(package)
      p <- progressor(along = children)
      pkgs <- lapply(children, FUN = function(child) {
        deps <- cran_revdeps(child)
        pkgs <- c(pkgs, deps)
        p(sprintf("%s (n=%d)", child, length(deps)))
      })
      pkgs <- unlist(pkgs, use.names = FALSE)
      pkgs <- unique(pkgs)
      pkgs <- setdiff(pkgs, package) ## WORKAROUND
      if (exclude_children) pkgs <- setdiff(pkgs, children)
      cache[[package]] <- pkgs
    }
    pkgs
  }
})


