#' @importFrom DBI dbWriteTable
#' @export
revdep_todo_reset <- function() {
  revdep_init()
  db <- revdepcheck:::db(".")
  df <- data.frame(package = character(0L), stringsAsFactors = FALSE)
  dbWriteTable(db, "todo", df, overwrite = TRUE, append = FALSE)
}
