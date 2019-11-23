import_from <- function(pkg, obj, mode = "function") {
  get(obj, mode = mode, envir = asNamespace(pkg))
}
