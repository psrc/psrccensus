.onAttach <- function(lib, pkg) {
  if (!interactive()) return()
  packageStartupMessage(
    "Have you updated tidycensus since the release date of the data you'll use?\n",
    "remotes::install_github('walkerke/tidycensus')"
  )
}
