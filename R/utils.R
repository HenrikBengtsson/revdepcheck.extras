import_from <- function(pkg, obj, mode = "function") {
  get(obj, mode = mode, envir = asNamespace(pkg))
}


is_symlink <- function(path) {
  file.exists(path) && !identical(Sys.readlink(path), "")
}

commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)
