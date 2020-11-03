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
  if (is.na(n)) n <- 1L
  
  min(c(n, availableCores()), na.rm = TRUE)
}
