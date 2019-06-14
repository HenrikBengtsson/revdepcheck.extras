#' Find all packages required by a specific package
#'
#' @param packages (character vector) Names of packages
#'
#' @param \ldots Not used.
#'
#' @importFrom crancache available_packages
#'
#' @export
required_packages <- function(packages, ...) {
  avail <- available_packages()
  stopifnot(all(packages %in% avail[, "Package"]))
  pkgs <- avail[packages, c("Depends", "Imports", "LinkingTo", "Enhances")]

  ## Drop version specifications
  pkgs <- gsub(" [(][^)]*[)]", "", pkgs)
  pkgs <- strsplit(pkgs, split = ", ", fixed = TRUE)
  pkgs <- sort(unique(unlist(pkgs)))
  
  ## Keep only packages that are available for installation
  ## This will drop 'R', 'base', 'tools', 'utils', ...
  pkgs <- intersect(pkgs, avail[, "Package"])

  pkgs
}

