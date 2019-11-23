#' @importFrom revdepcheck revdep_summary
#' @export
revdep_pkgs_with_status <- function(status = "error") {
  status <- match.arg(status)
  res <- revdep_summary()
  field <- switch(status, error = "errors")
  has_status <- vapply(res, FUN = function(x) {
    z <- x[["new"]][[field]]
    is.character(z) && any(nchar(z) > 0)
  }, FUN.VALUE = NA, USE.NAMES = TRUE)
  has_status <- !is.na(has_status) & has_status
  names(has_status)[has_status]
}
