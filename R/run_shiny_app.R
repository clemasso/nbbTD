#' Run Shiny App on the results
#'
#' Visualization of the results of class 'nbb.multiTD.output'
#'
#' @param rslt an object of class 'nbb.multiTD.output'
#' @import shiny
#' @export
#'
runShiny <- function(rslt) {

  # Define app location
  appDir <- system.file("shiny_app", "visualize_results", package = "nbbTD")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  # Data
  d_benchmarks <<- rslt$benchmarks
  d_indicators <<- rslt$indicators
  d_series_names <<- colnames(rslt$td_series)
  d_conversion <<- rslt$conversion
  d_bi_annual <<- rslt$bi_annual
  d_bi_annual_f <<- rslt$bi_annual_f
  d_bi_annual_falt <<- rslt$bi_annual_falt
  d_td_series <<- rslt$td_series
  d_td_series_stderr <<- rslt$td_series_stderr
  d_td_bi <<- rslt$td_bi
  d_td_bi_stderr <<- rslt$td_bi_stderr
  d_decomposition <<- rslt$decomposition
  d_fit <<- rslt$fit

  shiny::runApp(appDir, display.mode = "normal")
}


#' Growth chart (Shiny)
#' @export
plot_growth_chart <- function(benchmarks, indicators, series_name, call.conversion){

  # benchmark
  if (is.matrix(benchmarks)){
    y<-benchmarks[,series_name]
  } else {
    y<-benchmarks
  }

  # indicator
  if (is.matrix(indicators)){
    iname<-colnames(indicators)[grep(pattern = series_name, colnames(indicators))]
    x<-indicators[,iname]
    if (length(x)==0){
      x<-ts(rep(1,length(time(indicators))), start=start(indicators), frequency = frequency(indicators)) #smoothing
    } else if (!is.null(dim(x))){
      x<-x[,1] #only consider first indicator
    }
  } else {
    x<-indicators
  }

  x_in <- window(x, start = start(y), end = c(end(y)[1], frequency(x)))

  # compute annual growth rate
  if (grepl("Average", call.conversion, fixed = TRUE)){
    xT <- aggregate.ts(x_in, nfreq = 1, FUN = mean)
  } else {
    xT <- aggregate.ts(x_in, nfreq = 1, FUN = sum)
  }
  xT <- ts(xT, start = start(y))

  y_g <- y / lag(y,-1) - 1
  xT_g <- xT / lag(xT,-1) - 1

  # barplot
  grth <- rbind(y_g, xT_g)
  colnames(grth) <- start(y_g)[1]:end(y_g)[1]
  rownames(grth) <- c("benchmark", "indicator")
  barplot(grth,
          main="Annual growth rates: benchmark VS indicator",
          col=c("orange","darkgreen"),
          legend.text = rownames(grth),
          args.legend = list(x = "topleft"),
          beside=TRUE,
          yaxt="n")
  axis(1, at=seq(start(y)[1], end(y)[1], by = 1), las=2)
  axis(2, at=pretty(grth), lab=paste0(pretty(grth) * 100, " %"), las=TRUE)
}

#' Plot annual BI ratio (Shiny)
#'
#' @param bi_ts set of Bi ratio's. It must be a mts object.
#' @param f_bi_ts set of forecasted Bi ratio's. It must be a mts object.
#' @param series_name a character object. Name of the series to plot
#' @param scaled a boolean object. If scaled = TRUE, the BI ratio is scaled on
#'               to facilitate the interpretation of the change from year to year
#' @param \dots further arguments passed to or from other methods.
#' @export
plot_annual_bi <- function(bi_ts, f_bi_ts, series_name, scaled = FALSE, ...){

  # define first and end periods
  Y1 <- start(bi_ts)
  YN <- end(bi_ts)

  # select results
  bi_ts <- bi_ts[, series_name]
  f_bi_ts <- f_bi_ts[, series_name]
  nf <- length(f_bi_ts)

  if (scaled){
    bi_scaled_ts <- 100 * bi_ts / mean(bi_ts, na.rm = TRUE)
    bi_f_scaled_ts <- 100 * f_bi_ts / mean(bi_ts, na.rm = TRUE)
  }

  # plot results
  if (!scaled){
    ts.plot(bi_ts, gpars=list(xlab="", xaxt="n", ylab="", type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "blue", xlim = c(Y1[1], YN[1]+nf)), ylim = c(min(bi_ts, f_bi_ts, na.rm = TRUE), max(bi_ts, f_bi_ts, na.rm = TRUE)), ...)
    lines(f_bi_ts, type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "red")
    axis(1, at = seq(Y1[1], YN[1]+nf, by = 1), las=2)
  } else {
    ts.plot(bi_scaled_ts, gpars=list(xlab="", xaxt="n", ylab="", type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "blue", xlim = c(Y1[1], YN[1]+nf)), ylim = c(min(bi_scaled_ts, bi_f_scaled_ts, na.rm = TRUE), max(bi_scaled_ts, bi_f_scaled_ts, na.rm = TRUE)), ...)
    lines(bi_f_scaled_ts, type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "red")
    axis(1, at = seq(Y1[1], YN[1]+nf, by = 1), las=2)
  }
}

#' Disaggregated series with CI (Shiny)
#' @export
plot_td_series <- function(td_series, td_series_stderr, series_name){

  s <- td_series[,series_name]
  s_stderr <- td_series_stderr[,series_name]
  s_lb <- s - 1.96 * s_stderr
  s_ub <- s + 1.96 * s_stderr

  ts.plot(s, s_lb, s_ub,
          gpars=list(main = "Disaggregated series with CI", xlab="", ylab="", lty=c(1, 3, 3), xaxt="n", type = "o", pch=c(19,20,20), cex=0.8, las=2, col = "blue"))
  axis(1, at=seq(start(s)[1], end(s)[1], by = 1), las=2)
}

#' Disaggregated BI ratio with CI (Shiny)
#' @export
plot_td_bi <- function(td_bi, td_bi_stderr,series_name){

  bi <- td_bi[,series_name]
  bi_stderr <- td_bi_stderr[,series_name]
  bi_lb <- bi - 1.96 * bi_stderr
  bi_ub <- bi + 1.96 * bi_stderr

  ts.plot(bi, bi_lb, bi_ub,
          gpars=list(main = "Disaggregated BI ratio with CI", xlab="", ylab="",lty=c(1, 3, 3), xaxt="n", type = "o", pch=c(19,20,20), cex=0.8, las=2, col = "blue"))
  axis(1, at=seq(start(bi)[1], end(bi)[1], by = 1), las=2)
}

#' Plot decomposition of TD series (Shiny)
#' @export
plot_decomposition_td_series <- function(td_series, decompositions, series_name){

  s <- td_series[,series_name]
  reg_effect <- decompositions[[series_name]]$regeffect
  smoothing_effect <- s - reg_effect

  ts.plot(s, reg_effect, smoothing_effect, gpars=list(col=c("orange", "green", "blue"), xlab = "", xaxt="n", las=2))
  axis(side=1, at=start(s)[1]:end(s)[1])
  legend("topleft",c("disaggragated series", "regression effect", "smoothing effect"),lty = c(1,1,1), col=c("orange", "green", "blue"), bty="n", cex=0.8)
}
