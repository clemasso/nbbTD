
# Main class

#' Print function for objects of class 'nbb.dsc.td.multiproc.output'
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output'
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
print.nbb.dsc.td.multiproc.output <- function(x, ...){

  cat("Call:\n")
  print.default(x$call, ...)

  cat("\nModels:")
  print(table(x$model[,-1]))

  cat("\nComposition of the first-level output:\n")
  print.default(names(x), ...)

  cat("\nUse summary() for more details.")
}

#' Summary function for objects of class 'nbb.dsc.td.multiproc.output'
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output'
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
summary.nbb.dsc.td.multiproc.output <- function(x, ...){

  cat("Call:\n")
  print.default(x$call, ...)

  # number of processed series and number of observations
  cat(paste0("\nNumber of processed series: ", ncol(x$td.series), "\n"))
  cat(paste0("Number of observations of the low frequency benchmarks: ", nrow(x$bi.annual), "\n"))
  cat(paste0("Number of observations of the high frequency indicators: ", nrow(x$td.series), "\n"))

  # models
  cat("\nModel(s) used:\n")
  print(dcast(melt(x$model, id.vars = "series_name"), variable ~ series_name)[,-1])

  cat("\nComposition of the first-level output:\n")
  print.default(names(x), ...)
}


#' Plot function for objects of class 'nbb.dsc.td.multiproc.output'
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output'
#' @param series_name a character object with the name of the series
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
plot.nbb.dsc.td.multiproc.output <- function(x, series_name = "", ...){

  # if series_name missing
  if(series_name == ""){
    series_name <- colnames(x$td.series)[1]
    cat("The first series is plot by default. Change 'series_name' argument to plot results for another series.")
  }

  m <- x$model
  pos <- match(series_name, m$series_name)
  m_name <- as.character(m[pos, 2])

  par(mfrow=c(2,1), mar = c(3, 4, 3, 1.5))
  ts.plot(x$td.series[,series_name], gpars=list(main = series_name, xlab="", ylab="disaggragated series", type = "o", pch=19, cex=0.8, las=2, col = "blue"))
  ts.plot(x$bi.infra[,series_name], gpars=list(xlab="", ylab="BI ratio"))
  mtext(paste0("model: ", m_name), 3, line=2, cex=1)
}


# Sub-classes

#' Summary function for objects of sub-classes 'nbb.dsc.td.multiproc.output.denton' and 'nbb.dsc.td.multiproc.output.clvar'
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output.denton' or 'nbb.dsc.td.multiproc.output.clvar'
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
summary.nbb.dsc.td.multiproc.output.denton <- summary.nbb.dsc.td.multiproc.output.clvar <- function(x, ...){

  cat(paste0("\nNumber of processed series: ", ncol(x$td.series), "\n"))
  cat(paste0("Number of observations by series: ", nrow(x$td.series), "\n"))

  cat("\nComposition of the first-level output:\n")
  print.default(names(x), ...)
}


#' Plot function for objects of sub-classes 'nbb.dsc.td.multiproc.output.denton'
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output.denton'
#' @param series_name a character object with the name of the series
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
plot.nbb.dsc.td.multiproc.output.denton <- function(x, series_name = "", ...){

  # if series_name missing
  if(series_name == ""){
    series_name <- colnames(x$td.series)[1]
    cat("The first series is plot by default. Change 'series_name' argument to plot results for another series.")
  }

  td <- x$td.series[,series_name]
  bi <- x$bi.infra[,series_name]
  bi_sd <- x$bi.infra.SD[,series_name]
  bi_lb <- bi - 1.96 * bi_sd
  bi_ub <- bi + 1.96 * bi_sd
  td_lb <- bi_lb * td/bi
  td_ub <- bi_ub * td/bi

  ts.plot(td, td_lb, td_ub, gpars=list(main = paste0("Disaggragated series with CI ", series_name), lty=c(1, 3, 3), xlab="", ylab="", ...))
}


#' Plot function for objects of class 'nbb.dsc.td.multiproc.output.clvar'
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output.clvar'
#' @param series_name a character object with the name of the series
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
plot.nbb.dsc.td.multiproc.output.clvar <- function(x, series_name = "", ...){

  # if series_name missing
  if(series_name == ""){
    series_name <- colnames(x$td.series)[1]
    cat("The first series is plot by default. Change 'series_name' argument to plot results for another series.")
  }

  td_series <- x$td.series[,series_name]
  reg_effect <- x$details[[series_name]]$regeffect
  smoothing_effect <- td_series - reg_effect

  ts.plot(td_series, reg_effect, smoothing_effect, gpars=list(col=c("orange", "green", "blue"), xlab = "", xaxt="n", las=2, ...))
  axis(side=1, at=start(td_series)[1]:end(td_series)[1])
  legend("topleft",c("disaggragated series", "regression effect", "smoothing effect"),lty = c(1,1,1), col=c("orange", "green", "blue"), bty="n", cex=0.8)
}

