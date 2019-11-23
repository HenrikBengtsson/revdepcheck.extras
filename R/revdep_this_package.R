#' @importFrom desc desc
#' @export
revdep_this_package <- local({
  pkg <- NULL
  function() {
    if (is.null(pkg)) pkg <<- desc(file = "DESCRIPTION")$get("Package")
    pkg
  }
})
