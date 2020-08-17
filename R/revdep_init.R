#' Initiate the revdepcheck database
#'
#' @export
revdep_init <- function() {
  db_exists <- import_from("revdepcheck", "db_exists")
  db_setup <- import_from("revdepcheck", "db_setup")
  if (!db_exists(".")) db_setup(".")
}
