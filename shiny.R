
#' @export
deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  # Telling Shinyapps where to find packages
  # urls <- c(
  #   "energyandcleanair/202203_russian_gas",
  #   "energyandcleanair/rcrea")
  # remotes::install_github(urls, force=F, upgrade="never")

  # try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  options(rsconnect.http.trace = F)
  options(rsconnect.http.verbose = F)

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  rsconnect::deployApp(".",
                       appName=paste0("russia_shipments"),
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)
}
