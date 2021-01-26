#' Gets Packages Previously Checked
#'
#' @param file (character) Pathname to the \file{revdep/README.md}
#'
#' @return A data.frame with columns `package` and `version`.
#'
#' @export
revdep_readme_packages <- function(file = "revdep/README.md") {
  stopifnot(file_test("-f", file))
  x <- readLines(file)
  x <- x[-seq_len(grep("^# Revdep", x))]
  pattern <- "^[|](([[:alnum:]]+)|[[]([[:alnum:]]+))[^|]*[|]([[:digit:].-]+).*"
  x <- grep(pattern, x, value = TRUE)
  pkg <- gsub(pattern, "\\2\\3", x)
  ver <- package_version(gsub(pattern, "\\4", x))
  stopifnot(!anyNA(pkg), all(nzchar(pkg)), !anyNA(ver))
  data.frame(package = pkg, version = ver, stringsAsFactors = FALSE)
}
