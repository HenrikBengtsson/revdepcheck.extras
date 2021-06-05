#' Resets the set of reverse-package dependencies to be checked
#'
#' @param pkg (character) Path to package.
#'
#' @return Nothing.
#'
#' @importFrom DBI dbWriteTable
#' @export
revdep_todo_reset <- function(pkg = ".") {
  revdepcheck_db <- import_from("revdepcheck", "db")
  revdep_init()
  db <- revdepcheck_db(pkg)
  df <- data.frame(package = character(0L), stringsAsFactors = FALSE)
  dbWriteTable(db, "todo", df, overwrite = TRUE, append = FALSE)
}
