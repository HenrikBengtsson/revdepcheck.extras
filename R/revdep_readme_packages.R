#' Get Information of Packages Previously Checked
#'
#' @param file (character) Pathname to the \file{revdep/README.md}
#'
#' @param available (character matrix) An [utils::available.packages()] matrix, or NULL
#'
#' @return A data.frame with columns
#' `package` (character),
#' `version` (package_version), and `repo_version` (package_version).
#' A `repo_version` of `0.0.0.0` indicates that the package is not available
#' in upstream repositories.
#' The `repo_version` column is not included if `available` is NULL.
#'
#' @examples
#' \dontrun{\donttest{
#' ## Packages previously checked
#' pkgs <- revdep_readme_packages()
#' print(head(pkgs))
#'
#' ## Checked packages that have since been updated
#' old <- subset(pkgs, version < repo_version)
#' print(old)
#' }}
#'
#' @importFrom utils available.packages
#' @export
revdep_readme_packages <- function(file = "revdep/README.md", available = available.packages()) {
  ## To please 'R CMD check'
  repo_version <- NULL
  
  stopifnot(file_test("-f", file))
  stopifnot(
    is.null(available) ||
    (is.matrix(available) && is.character(available))
  )

  x <- readLines(file)
  x <- x[-seq_len(grep("^##[[:space:]]+All", x))]
  pattern <- "^[|](([[:alnum:].]+)|[[]([[:alnum:].]+))[^|]*[|]([[:digit:].-]+).*"
  x <- grep(pattern, x, value = TRUE)
  pkg <- gsub(pattern, "\\2\\3", x)
  ver <- package_version(gsub(pattern, "\\4", x))
  stopifnot(
    !anyNA(pkg), all(nzchar(pkg)), !any(duplicated(pkg)),
    !anyNA(ver)
  )
  o <- order(pkg)
  pkg <- pkg[o]
  ver <- ver[o]

  pkgs <- data.frame(package = pkg, version = ver, stringsAsFactors = FALSE)

  if (!is.null(available)) {
    ## Get version available in upstream package repositories
    repo_version <- package_version("0.0.0.0")  ## non-existing
    avail <- (pkg %in% available[,"Package"])
    repo_version[avail] <- available[pkg[avail], "Version"]
    pkgs$repo_version <- repo_version
  }
  
  pkgs
}
