#' Get The Number of Concurrent Check Workers
#'
#' @return The number of concurrent workers allowed on the current machine.
#'
#' @importFrom parallelly availableCores
#' @export
num_workers <- function() {
  getenv <- function(name) {
    as.integer(Sys.getenv(name, NA_character_))
  }
  getopt <- function(name) {
    as.integer(getOption(name, NA_integer_))
  }
  
  n <- getopt("revdepcheck.num_workers")
  if (is.na(n)) n <- getenv("R_REVDEPCHECK_NUM_WORKERS")
  
  if (is.na(n)) {
    warning("R_REVDEPCHECK_NUM_WORKERS/revdepcheck.num_workers not set; using a single worker")
    return(1L)
  }

  max <- availableCores()
  if (max < n) {
    warning(sprintf("Be careful. You requested %d workers (via R_REVDEPCHECK_NUM_WORKERS/revdepcheck) but parallelly::availableCores() reports %s=%d so you will be running more concurrent 'R CMD check' processes than then number of cores alloted to you", n, names(max), max), immediate. = TRUE)
  }

  n
}
