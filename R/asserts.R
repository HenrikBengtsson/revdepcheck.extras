assert_repos <- function(verbose = FALSE) {
  repos <- getOption("repos")
  
  repos_info <- sprintf("%s=%s", names(repos), dQuote(repos))
  repos_info <- paste(repos_info, collapse = ", ")
  
  if (verbose) {
    message("Repositories (option 'repos'):")
    message(paste(paste0(" - ", repos_info, collapse = "\n")))
  }
  
  if (any(grepl("^@.*@$", repos))) {
    stop("Option 'repos' has unset repositories: ", repos_info)
  }
  
  invisible(repos)
}
