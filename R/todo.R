#' @importFrom revdepcheck revdep_todo
#' @export
todo <- function() {
  pkgs <- tryCatch(revdep_todo(), error = function(ex) NA)
  if (identical(pkgs, NA)) {
    cat("Revdepcheck has not been initiated\n")
    return()
  }

  status <- NULL  ## To please R CMD check
  
  pkgs <- subset(pkgs, status == "todo")
  if (nrow(pkgs) == 0) {
    cat("There are no packages on the revdepcheck todo list\n")
  } else {
    cat(sprintf("%d. %s\n", seq_len(nrow(pkgs)), pkgs$package))
  }
}
