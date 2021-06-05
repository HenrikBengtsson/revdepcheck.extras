#' Get all the reverse-dependency packages to be checked
#'
#' @param pkg (character) Path to package.
#'
#' @param print If TRUE, the list is printed, otherwise not.
#'
#' @return (character vector; invisible) The name of the
#' packages to be tested.
#'
#' @importFrom revdepcheck revdep_todo
#' @export
todo <- function(pkg = ".", print = TRUE) {
  pkgs <- tryCatch(revdep_todo(pkg), error = function(ex) NA)
  if (identical(pkgs, NA)) {
    cat("Revdepcheck has not been initiated\n")
    return(invisible(character(0L)))
  }

  status <- NULL  ## To please R CMD check
  
  pkgs <- subset(pkgs, status == "todo")$package

  if (print) {
    if (length(pkgs) == 0) {
      cat("There are no packages on the revdepcheck todo list\n")
    } else {
      width <- floor(log10(length(pkgs))) + 1L
      cat(sprintf("%*d. %s\n", width, seq_along(pkgs), pkgs))
    }
  }

  invisible(pkgs)
}
