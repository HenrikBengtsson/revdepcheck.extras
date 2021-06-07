#' Get packages with a certain check status
#'
#' @param pkg (character) Path to package.
#'
#' @param status (character vector) A status to filter checked
#' packages by.
#' If `"error"`, then packages for which the 'new' version failed are returned.
#' If `"failure"`, then packages with an internal "E" status are returned.
#'
#' @return (character vector) Zero or more package names.
#'
#' @importFrom revdepcheck revdep_summary
#' @export
revdep_pkgs_with_status <- function(pkg = ".", status = c("error", "failure")) {
  status <- match.arg(status)
  res <- revdep_summary(pkg)
  if (status == "failure") {
    names(which(sapply(res, FUN = .subset2, "status") == "E"))
  } else if (status == "error") {
    field <- switch(status, error = "errors")
    has_status <- vapply(res, FUN = function(x) {
      z <- x[["new"]][[field]]
      is.character(z) && any(nchar(z) > 0)
    }, FUN.VALUE = NA, USE.NAMES = TRUE)
    has_status <- !is.na(has_status) & has_status
    names(has_status)[has_status]
  }
}
