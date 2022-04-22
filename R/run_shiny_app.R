#' Run Shiny App on the results
#'
#' Visualization of the results of class 'nbb.dsc.td.multiproc.output'
#'
#' @param res an object of class 'nbb.dsc.td.multiproc.output'
#' @import shiny
#' @export
#'
runShiny <- function(res) {

  # Define app location
  appDir <- system.file("shiny_app", "visualize_results", package = "nbbTD")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  # Data
  ## General
  d_call <<- res$call
  d_series_names <<- colnames(res$td.series)
  d_benchmarks <<- res$benchmarks
  d_indicators <<- res$indicators
  d_td_series <<- res$td.series
  d_bi_infra <<- res$bi.infra
  d_bi_annual <<- res$bi.annual
  d_bi_annual_f <<- res$bi.annual.f
  d_bi_annual_falt <<- res$bi.annual.falt

  ## Specific
  d_edenton <<- res$eDenton
  d_denton <<- res$Denton
  d_clvar <<- res$CLvar

  shiny::runApp(appDir, display.mode = "normal")
}
