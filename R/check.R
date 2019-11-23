#' @importFrom utils file_test
#' @importFrom revdepcheck revdep_check
#' @export
check <- function() {
  if (file_test("-f", p <- Sys.getenv("R_CHECK_ENVIRON", "~/.R/check.Renviron"))) {
    cat(sprintf("R CMD check will use env vars from %s\n", sQuote(p)))
    cat(sprintf("To disable, set 'R_CHECK_ENVIRON=false' (a fake pathname)\n"))
  }

  envs <- Sys.getenv()
  envs <- envs[grep("^_?R_CHECK_", names(envs))]
  if (length(envs) > 0L) {
    envs <- sprintf(" %02d. %s=%s", seq_along(envs), names(envs), envs)
    envs <- paste(envs, collapse="\n")
    cat(sprintf("Detected R-specific env vars that may affect R CMD check:\n%s\n", envs))
  }

  if (exists("precheck", mode = "function", envir = .GlobalEnv)) {
    precheck <- get("precheck", mode = "function", envir = .GlobalEnv)
    precheck()
  }
  
  revdep_check(bioc = TRUE, num_workers = available_cores(),
               timeout = as.difftime(30, units = "mins"), quiet = FALSE)
}
