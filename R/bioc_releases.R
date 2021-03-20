#' Gets the Version and Release Date for All Bioconductor Releases
#'
#' @return A data.frame.
#'
#' @export
bioc_releases <- function() {
  config <- yaml::read_yaml("https://bioconductor.org/config.yaml")
  data.frame(
    version = package_version(names(config$release_dates)),
    date = as.Date(unlist(config$release_dates), format="%m/%d/%Y")
  )
}
