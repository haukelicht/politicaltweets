.onLoad <- function(libname, pkgname) {
  options("politicaltweets.cache.path" = file.path(tryCatch(path.package("politicaltweets"), error = function(err) getwd()), "cache"))
}

.onAttach <- function(libname, pkgname) {
  startup_msg <- "\033[1mpoliticaltweets\033[22m: Classifying tweets according to whether or not they are “political” using a pre-trained ensemble model\n"

  if (!rlang::is_installed("laserize")) {
    note <- paste(
      paste("\nNote: It seems that the", sQuote("laserize"), "R package is not installed and setup on your system.")
      , 'Run `remotes::install_github("haukelicht/laserize")` and refer to `?laserize::setup_laser`'
      , "in case you want to use the politicaltweets::create_tweet_text_representations()."
      , ""
      , sep = "\n"
    )
  } else {
    note <- ""
  }
  startup_msg <- paste0(startup_msg, note)

  packageStartupMessage(startup_msg)
}

.onUnload <- function(libname, pkgname) {
  res <- lapply(list.files(getOption("politicaltweets.cache.path"), full.names = TRUE), file.remove)
  .Options$politicaltweets.cache.path <- NULL
}
