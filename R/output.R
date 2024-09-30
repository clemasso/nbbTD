#' Print function for objects of class 'nbb.multiTD.output'
#'
#' @param x an object of class 'nbb.multiTD.output'
#' @param \dots further elements to display.
#' @export
#'
print.nbb.multiTD.output <- function(x, ...){

  print(list(call=x$call,
             td_series=x$td_series,
             bi_ratio=x$td_bi,...))
}

#' Summary function for objects of class 'nbb.multiTD.output'
#'
#' @param object an object of class 'nbb.multiTD.output'
#' @param \dots further elements to display.
#' @export
#'
summary.nbb.multiTD.output <- function(object, ...){

  print(list(fit=object$fit,...))

}


#' Plot function for objects of class 'nbb.multiTD.output'
#'
#' @param x an object of class 'nbb.multiTD.output'
#' @param series_name a character object with the name of the series. Must be
#'   kept to NULL for single processing.
#'
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
plot.nbb.multiTD.output <- function(x, series_name = NULL, ...){

  if (is.null(series_name)){
    col_num<-1
    series_name<-if (is.matrix(x$benchmarks)) colnames(x$benchmarks)[1] else "series"
  } else{
    col_num<-match(series_name, colnames(x$benchmarks))
    if (is.na(col_num)) stop("Series name not found")
  }

  s<-x$td_series[,col_num]
  s_stderr<-x$td_series_stderr[,col_num]
  s_lb <- s - 1.96 * s_stderr
  s_ub <- s + 1.96 * s_stderr

  bi <- x$td_bi[,col_num]
  bi_stderr <- x$td_bi_stderr[,col_num]
  bi_lb <- bi - 1.96 * bi_stderr
  bi_ub <- bi + 1.96 * bi_stderr

  par(mfrow=c(2,1), mar = c(3, 4, 3, 1.5))
  ts.plot(s, s_lb, s_ub,
          gpars=list(main=paste0(series_name, ": Disaggregated series with confidence interval"), xlab="", ylab= "", lty=c(1, 3, 3), xaxt="n", type = "o", pch=c(19,20,20), cex=0.8, las=2, col = "blue"))
  axis(1, at=seq(start(s)[1], end(s)[1], by = 1), las=2)

  ts.plot(bi, bi_lb, bi_ub,
          gpars=list(main=paste0(series_name, ": Disaggregated BI ratio with confidence interval"), xlab="", ylab="",lty=c(1, 3, 3), xaxt="n", type = "o", pch=c(19,20,20), cex=0.8, las=2, col = "blue"))
  axis(1, at=seq(start(bi)[1], end(bi)[1], by = 1), las=2)
}
