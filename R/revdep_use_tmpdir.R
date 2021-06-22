#' Place redepcheck's 'check', 'library', and '.cache' folders in a temporary folder
#'
#' @param pkg (character) Path to package.
#'
#' @param tmpdir (character) The temporary folder in which revdep subfolders
#' should be hosted
#'
#' @return (character vector; invisible) The path to to the three
#' revdep/{check,library,.cache} folders.
#'
#' @details
#' This function will create folders:
#'
#'   1. \file{<tmpdir>/revdepcheck/<package>/revdep/check}
#'   2. \file{<tmpdir>/revdepcheck/<package>/revdep/library}
#'   3. \file{<tmpdir>/revdepcheck/<package>/revdep/.cache}
#'
#' which are linked to from:
#'
#'   1. \file{<pkg>/revdep/check}
#'   2. \file{<pkg>/revdep/library}
#'   3. \file{<pkg>/revdep/.cache}
#'
#' using symbolic links.  If the `tmpdir` folder is on a fast, local drive,
#' then [revdepcheck::revdep_check()] will run faster compared to if
#' using folders on a, say, slow, heavily-loaded parallel file system.
#' To utilize the \file{revdep/.cache} folder, set environment variables
#' \env{R_USER_CACHE_DIR=$PWD/revdep/.cache} and then
#' \env{XDG_CACHE_HOME=$R_USER_CACHE_DIR}. Function [check()] sets these
#' if it detects a \file{revdep/.cache} folder.
#'
#' @importFrom utils file_test
#' @export
revdep_use_tmpdir <- function(pkg = ".", tmpdir = dirname(tempdir())) {
  root <- "revdep"
  if (!file_test("-d", root)) dir.create(root, recursive = TRUE)
  stopifnot(file_test("-d", root))
  stopifnot(file_test("-d", tmpdir))

  pkgname <- if (pkg == ".") revdep_this_package() else basename(pkg)
  from_root <- file.path(tmpdir, .packageName, pkgname, root)
  if (!file_test("-d", from_root)) dir.create(from_root, recursive = TRUE)
  stopifnot(file_test("-d", from_root))

  dirs <- c("checks", "library", ".cache")
  for (dir in dirs) {
    to <- file.path(root, dir)
    if (!is_symlink(to)) {
      stopifnot(!file_test("-d", to))
      from <- file.path(from_root, dir)
      if (!file_test("-d", from)) dir.create(from, recursive = TRUE)
      stopifnot(file_test("-d", from))
      file.symlink(from = from, to = to)
    }
  }

  invisible(file.path(root, dirs))
}
