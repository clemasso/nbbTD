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
    stop("Could not find directory. Try re-installing `mypackage`.", call. = FALSE)
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


### Various plot functions run by Shiny app

#' Growth chart (Shiny)
#' @export
#'
plot_growth_chart <- function(benchmarks, indicators, series_name, call.conversion){

  y <- benchmarks[,series_name]
  x <- indicators[,series_name]
  x_in <- window(x, start = start(y), end = c(end(y)[1], frequency(x)))

  if(grepl("Average", call.conversion, fixed = TRUE)){
    xT <- aggregate.ts(x_in, nfreq = 1, FUN = mean)
  } else{
    xT <- aggregate.ts(x_in, nfreq = 1, FUN = sum)
  }
  xT <- ts(xT, start = start(y))

  y_g <- y / lag(y,-1) - 1
  xT_g <- xT / lag(xT,-1) - 1

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


#' Plot TD series (Shiny)
#' @export
#'
plot_td_series <- function(td_series, series_name){

  x <- td_series[,series_name]

  ts.plot(x, gpars=list(xlab="", ylab="", xaxt="n", type = "o", pch=19, cex=0.8, las=2, col = "blue",
                        main = "Disaggregated series"))
  axis(1, at=seq(start(x)[1], end(x)[1], by = 1), las=2)
}

#' Plot infra-annual BI (Shiny)
#' @export
#'
plot_bi_infra <- function(bi_infra, series_name, bi_infra_f = NULL){

  x <- bi_infra[,series_name]

  if(is.null(bi_infra_f)){
    ts.plot(x, gpars=list(xlab="", ylab="", xaxt="n", type = "o", pch=19, cex=0.8, las=2, col = "blue",
                          main = "Infra-annual BI ratio"))
    axis(1, at=seq(start(x)[1], end(x)[1], by = 1), las=2)
  } else{
    x_f <- bi_infra_f[, series_name]
    freq <- frequency(bi_infra)
    Nf <- ceiling(length(x_f)/freq)

    ts.plot(x, gpars=list(xlab="", ylab="", xaxt="n", type = "o", pch=19, cex=0.8, las=2, col = "blue",
                          main = "Infra-annual BI ratio"),
            xlim = c(start(x)[1], end(x)[1]+Nf+1),
            ylim = c(min(x, x_f), max(x, x_f)))
    lines(x_f, type = "o", lwd=2, pch=19, cex=0.8, las=2, col = "red")
    axis(1, at = seq(start(x)[1], end(x)[1]+Nf, by = 1), las=2)
  }
}

#' Plot (e)Denton TD series with confidence intervals (Shiny)
#' @param x an object of class 'nbb.dsc.td.multiproc.output.denton'
#' @param series_name a character object with the name of the series
#' @export
#'
plot_td_series_ci <- function(x, series_name){

  td <- x$td.series[,series_name]
  bi <- x$bi.infra[,series_name]
  bi_sd <- x$bi.infra.SD[,series_name]
  bi_lb <- bi - 1.96 * bi_sd
  bi_ub <- bi + 1.96 * bi_sd
  td_lb <- bi_lb * td/bi
  td_ub <- bi_ub * td/bi

  ts.plot(td, td_lb, td_ub, gpars=list(main = paste0("Disaggragated series with CI ", series_name), lty=c(1, 3, 3), xlab="", ylab=""))
}

#' Plot (e)Denton: comparison BI ratio with/without manual input (Shiny)
#' @export
#'
plot_bi_compare_manual <- function(res_denton, series_name){

  bi_NONM <- res_denton$bi.infra.NONM[,series_name]
  bi_NO <- res_denton$bi.infra.NO[,series_name]
  bi_NM <- res_denton$bi.infra.NM[,series_name]
  bi <- res_denton$bi.infra[,series_name]
  pos_legend <- ifelse(bi[1] > bi[length(bi)], "bottomleft", "topleft")

  ts.plot(bi_NONM, bi_NO, bi_NM, bi,gpars=list(col=c("lightblue", "lightgreen", "lightsalmon", "blue"), xlab = "", xaxt="n", las=2, type = "o", pch=19, cex=0.7, main = "Infra-annual BI ratio"))
  axis(1, at=seq(start(bi)[1], end(bi)[1], by = 1), las=2)
  legend(pos_legend, c("no outlier - no manual", "no outlier - with manual", "no manual - with outliers", "with outliers and manual"),lty = c(1,1,1,1), pch =c(19,19,19,19), col=c("lightblue", "lightgreen", "lightsalmon", "blue"), bty="n", cex=0.8)
}

#' Plot (e)Denton: comparison TD with/without manual input (Shiny)
#' @export
#'
plot_td_compare_manual <- function(res_denton, series_name, indicators){

  indicator <- indicators[,series_name]
  bi_NONM <- res_denton$bi.infra.NONM[,series_name]
  bi_NO <- res_denton$bi.infra.NO[,series_name]
  bi_NM <- res_denton$bi.infra.NM[,series_name]

  td <- res_denton$td.series[,series_name]
  td_NONM <- bi_NONM * indicator
  td_NO <- bi_NO * indicator
  td_NM <- bi_NM * indicator

  pos_legend <- ifelse(td[1] > td[length(td)], "bottomleft", "topleft")

  ts.plot(td_NONM, td_NO, td_NM, td,gpars=list(col=c("lightblue", "lightgreen", "lightsalmon", "blue"), xlab = "", xaxt="n", las=2, type = "o", pch=19, cex=0.7, main = "Disaggregated series"))
  axis(1, at=seq(start(td)[1], end(td)[1], by = 1), las=2)
  legend(pos_legend, c("no outlier - no manual", "no outlier - with manual", "no manual - with outliers", "with outliers and manual"),lty = c(1,1,1,1), pch =c(19,19,19,19), col=c("lightblue", "lightgreen", "lightsalmon", "blue"), bty="n", cex=0.8)
}

#' Plot decomposition of CLvar TD series (Shiny)
#'
#' @param x an object of class 'nbb.dsc.td.multiproc.output.clvar'
#' @param series_name a character object with the name of the series
#' @export
#'
plot_td_series_decomp <- function(x, series_name){

  td_series <- x$td.series[,series_name]
  reg_effect <- x$details[[series_name]]$regeffect
  smoothing_effect <- td_series - reg_effect

  ts.plot(td_series, reg_effect, smoothing_effect, gpars=list(col=c("orange", "green", "blue"), xlab = "", xaxt="n", las=2))
  axis(side=1, at=start(td_series)[1]:end(td_series)[1])
  legend("topleft",c("disaggragated series", "regression effect", "smoothing effect"),lty = c(1,1,1), col=c("orange", "green", "blue"), bty="n", cex=0.8)
}



