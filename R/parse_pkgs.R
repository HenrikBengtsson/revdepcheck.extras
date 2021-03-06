#' Extract packages names from messy strings
#'
#' @param pkgs (character vector) Extract package names
#' from one or more strings with our without commas, spaces,
#' quotation marks, etc.
#'
#' @return (character vector) A sorted set of unique package
#' names.
#'
#' @export
parse_pkgs <- function(pkgs) {
  pkgs <- unlist(strsplit(pkgs, split = ",", fixed = TRUE))
  fqL <- rawToChar(as.raw(c(0xe2, 0x80, 0x98)))
  fqR <- rawToChar(as.raw(c(0xe2, 0x80, 0x99)))
  drop <- c(" ", "\t", "'", '"', fqL, fqR)
  pattern <- sprintf("[%s]", paste(drop, collapse=""))
  pkgs <- gsub(pattern, "", pkgs)
  sort(unique(pkgs))
}
