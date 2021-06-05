#' Initiate the revdepcheck database
#'
#' @param pkg (character) Path to package.
#'
#' @return Nothing.
#'
#' @export
revdep_init <- function(pkg = ".") {
  db_exists <- import_from("revdepcheck", "db_exists")
  db_setup <- import_from("revdepcheck", "db_setup")
  dbenv <- import_from("revdepcheck", "dbenv", mode = "environment")
  
  if (!db_exists(pkg)) {
    if (exists(pkg, envir = dbenv, inherits = TRUE)) {
      rm(list = pkg, envir = dbenv)
    }
    db_setup(pkg)
  }
}
