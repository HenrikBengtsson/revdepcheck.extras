#' Checks whether or not a package is already in the binary crancache
#'
#' @param pkgs (character vector) Zero or more package names
#'
#' @return A named logical vector indicating whether or not
#' each package is in the binary crancache.
#'
#' @importFrom crancache crancache_list available_packages
#' @export
is_in_crancache <- function(pkgs) {
  # To please R CMD check
  Repository <- Package <- Version <- NULL
  
  res <- logical(length = length(pkgs))
  names(res) <- pkgs

  if (length(pkgs) == 0L) return(res)

  done <- crancache_list()
  done <- subset(done, Package %in% pkgs & grepl("-bin/source$", Repository))[,c("Package", "Version")]
  rownames(done) <- NULL
  res[unique(done$Package)] <- TRUE
  
  avail <- available_packages()
  avail_pkgs <- intersect(names(res)[res], rownames(avail))
  for (pkg in avail_pkgs) {
    avail_ver <- package_version(avail[pkg, "Version"])
    done_vers <- package_version(done$Version[done$Package == pkg])
    if (!any(done_vers >= avail_ver, na.rm = TRUE)) res[pkg] <- FALSE
  }

  res
}
