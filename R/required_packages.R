#' Find all packages required by a specific package
#'
#' @param packages (character vector) Names of packages.
#'
#' @param \ldots Not used.
#'
#' @return (character vector) Zero or more package names.
#'
#' @importFrom crancache available_packages
#'
#' @export
required_packages <- function(packages, ...) {
  if (length(packages) == 0L) return(character(0L))

  avail <- available_packages()
  stopifnot(all(packages %in% avail[, "Package"]))
  pkgs <- avail[packages, c("Depends", "Imports", "LinkingTo", "Enhances")]
  pkgs <- pkgs[!is.na(pkgs)]
  if (length(pkgs) == 0L) return(character(0L))

  ## Drop version specifications
  pkgs <- gsub("[[:space:]]*[(][^)]*[)]", "", pkgs)
  pkgs <- unlist(strsplit(pkgs, split = "[,[:space:]]+"))
  pkgs <- sort(unique(pkgs[nzchar(pkgs)]))
  
  ## Keep only packages that are available for installation
  ## This will drop 'R', 'base', 'tools', 'utils', ...
  pkgs <- intersect(pkgs, avail[, "Package"])

  pkgs
}

