#' Get the name of package whose reverse dependencies should be checked
#'
#' @return (character string) The package name.
#'
#' @importFrom desc desc
#' @export
revdep_this_package <- local({
  pkg <- NULL
  function() {
    if (is.null(pkg)) pkg <<- desc(file = "DESCRIPTION")$get("Package")
    pkg
  }
})
