#' @export
revdep_init <- function() {
  if (!revdepcheck:::db_exists(".")) revdepcheck:::db_setup(".")
}
