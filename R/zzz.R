.onAttach <- function(lib, pkg){
  msg <- base::writeLines(c("Have you updated tidycensus since the release date of the data you'll use?",
                            "devtools::install_github('walkerke/tidycensus')"))
  packageStartupMessage(msg)
  invisible()
}
