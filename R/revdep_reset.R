#' Resets revdep check folders
#'
#' @param pkg (character) Path to package.
#'
#' @return Nothing.
#'
#' @importFrom utils file_test
#' @export
revdep_reset <- function(pkg = ".") {
  root <- file.path(pkg, "revdep")
  dirs <- c("cache", "checks", "library")
  for (dir in dirs) {
    to <- file.path(root, dir)
    if (is_symlink(to)) {
      parts <- dir(path = to, all.files = TRUE, full.names = TRUE, no.. = TRUE)
      unlink(parts, recursive = TRUE)
      unlink(to)
    } else if (file_test("-d", to)) {
      unlink(to, recursive = TRUE)
    }
  }

  revdepcheck::revdep_reset(pkg = pkg)
  invisible()
}
