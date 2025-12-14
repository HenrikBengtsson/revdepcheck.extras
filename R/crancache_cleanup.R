#' Clean up the local crancache
#'
#' Looks for packages in the local crancache that are no longer available
#' online. For example, this removes recently archive CRAN packages and
#' Bioconductor package no longer available.
#'
#' @param dryrun If TRUE, the cache is not cleaned up.
#'
#' @return
#' Invisibly returns currently cached packages.
#'
#' @importFrom crancache available_packages crancache_remove
#' @importFrom utils available.packages
#' @export
crancache_cleanup <- function(dryrun = FALSE) {
  ## Scan 'repos' repositories for available package (online)
  avail <- available.packages()
  message(sprintf("Number of available package (online): %d", nrow(avail)))
  
  ## Scan local crancache for available packages
  avail_local <- available_packages()
  message(sprintf("Number of cached package (before): %d", nrow(avail_local)))

  ## Packages in local crancache not online
  extra <- unique(setdiff(avail_local[,"Package"], avail[,"Package"]))
  message(sprintf("Cached packages not available online (before): [n=%d] %s", length(extra), commaq(extra)))

  if (!dryrun) {
    ## Remove extra packages from local crancache
    crancache_remove(extra)
  
    ## Scan local crancache for available packages
    avail_local <- available_packages()
    message(sprintf("Number of cached package (after): %d", nrow(avail_local)))
  
    ## Packages in local crancache not online
    extra <- unique(setdiff(avail_local[,"Package"], avail[,"Package"]))
    message(sprintf("Cached packages not available online (after): [n=%d] %s", length(extra), commaq(extra)))
  }

  invisible(avail_local)
}
