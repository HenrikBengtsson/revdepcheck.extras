#' Place revdepcheck's 'cache', 'checks', and 'library' folders in a temporary folder
#'
#' @param pkg (character) Path to package.
#'
#' @param tmpdir (character) The temporary folder in which revdep subfolders
#' should be hosted
#'
#' @return (character vector; invisible) The paths to the three
#' revdep/\{cache,checks,library\} folders.
#'
#' @details
#' This function will create folders:
#'
#'   1. \file{<tmpdir>/revdepcheck.extras/<package>/revdep/cache}
#'   2. \file{<tmpdir>/revdepcheck.extras/<package>/revdep/checks}
#'   3. \file{<tmpdir>/revdepcheck.extras/<package>/revdep/library}
#'
#' which are linked to from:
#'
#'   1. \file{<pkg>/revdep/cache}
#'   2. \file{<pkg>/revdep/checks}
#'   3. \file{<pkg>/revdep/library}
#'
#' using symbolic links.  If the `tmpdir` folder is on a fast, local drive,
#' then [revdepcheck::revdep_check()] will run faster compared to if
#' using folders on a, say, slow, heavily-loaded parallel file system.
#' To utilize the \file{revdep/cache} folder, set environment variables
#' \env{R_USER_CACHE_DIR=$PWD/revdep/cache} and then
#' \env{XDG_CACHE_HOME=$R_USER_CACHE_DIR}. Function [check()] sets these
#' if it detects a \file{revdep/cache} folder.
#'
#' @importFrom utils file_test
#' @export
revdep_use_tmpdir <- function(pkg = ".", tmpdir = dirname(tempdir())) {
  root <- file.path(pkg, "revdep")
  if (!file_test("-d", root)) dir.create(root, recursive = TRUE)
  stopifnot(file_test("-d", root))
  stopifnot(file_test("-d", tmpdir))

  pkgname <- if (pkg == ".") revdep_this_package() else basename(pkg)
  from_root <- file.path(tmpdir, .packageName, pkgname, "revdep")
  if (!file_test("-d", from_root)) dir.create(from_root, recursive = TRUE)
  stopifnot(file_test("-d", from_root))

  dirs <- c("cache", "checks", "library")
  for (dir in dirs) {
    to <- file.path(root, dir)
    from <- file.path(from_root, dir)
    if (!file_test("-d", from)) dir.create(from, recursive = TRUE)
    stopifnot(file_test("-d", from))
    if (is_symlink(to)) {
      if (!file_test("-d", to)) {
        unlink(to)
        file.symlink(from = from, to = to)
      }
    } else {
      stopifnot(!file_test("-d", to))
      file.symlink(from = from, to = to)
    }
  }

  invisible(file.path(root, dirs))
}
