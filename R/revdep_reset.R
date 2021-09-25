#' Resets revdep check folders
#'
#' @param pkg (character) Path to package.
#'
#' @return Nothing.
#'
#' @importFrom utils file_test
#' @export
revdep_reset <- function(pkg = ".") {
  root <- "revdep"
  ## Nothing todo?
  if (!file_test("-d", root)) return(invisible())

  dirs <- c("cache", "checks", "library")
  for (dir in dirs) {
    to <- file.path(root, dir)
    if (is_symlink(to)) {
      parts <- dir(path = to, all.files = TRUE, full.names = TRUE)
      unlink(parts, recursive = TRUE)
    }
    unlink(to, recursive = TRUE)
  }

  invisible(invisible())
}
