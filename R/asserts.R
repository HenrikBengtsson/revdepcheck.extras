assert_repos <- function(verbose = FALSE) {
  repos <- getOption("repos")
  
  names <- names(repos)
  if (is.null(names)) {
    repos_info <- dQuote(repos)
  } else {
    repos_info <- ifelse(nzchar(names), sprintf("%s=%s", names, dQuote(repos)), dQuote(repos))
  }
  
  if (verbose) {
    message("Repositories (option 'repos'):")
    message(paste0(" - ", repos_info, collapse = "\n"))
  }
  
  if (any(grepl("^@.*@$", repos))) {
    repos_info <- paste(repos_info, collapse = ", ")
    stop("Option 'repos' has unset repositories: ", repos_info)
  }
  
  invisible(repos)
}
