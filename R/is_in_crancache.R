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
  
  if (length(pkgs) == 0L) return(character(0L))

  res <- logical(length = length(pkgs))
  names(res) <- pkgs

  done <- crancache_list()
  done <- subset(done, Package %in% pkgs & grepl("-bin/source$", Repository))[,c("Package", "Version")]
  rownames(done) <- NULL
  res[done$Package] <- TRUE
  
  avail <- as.data.frame(available_packages())
  for (kk in seq_len(nrow(done))) {
    p <- done[kk,]
    a <- subset(avail, Package == p$Package)
    a <- subset(a, Version > p$Version)
    if (any(p$Version < a$Version)) res[p$Package] <- FALSE
  }

  res
}
