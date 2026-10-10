import_from <- function(pkg, obj, mode = "function") {
  get(obj, mode = mode, envir = asNamespace(pkg))
}


is_symlink <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) && nzchar(target)
}

commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)
