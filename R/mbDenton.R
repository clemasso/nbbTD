#' Model-based Denton
#'
#' Proportional First Difference Denton method put in State Space form and
#' including the possibility to add outliers (shifts in the disaggregated BI
#' ratio) and manual figures for the disaggregated BI ratio.
#'
#' @param indicator monthly or quarterly indicator. Must be a ts object.
#' @param benchmark annual benchmark. Must be a ts object.
#' @param outliers level shift(s) in the Benchmark-to-Indicator ratio. Must be
#'   specified as a numeric vector with the outlier periods in decimal dates.
#'   Default is NULL (no outlier).
#' @param outliers.intensity intensity of the  outlier, defined as the relative
#'   value of the 'innovation variances' (1= normal situation). Must be
#'   specified as a numeric vector corresponding to the outlier periods
#'   specified in the outliers argument. Default is 10 for each outlier.
#' @param manual_disagBI Disaggregated BI ratio that the user wants to fix. Must
#'   be as a matrix of two columns called 'period' and 'bi_ratio'. The periods
#'   must be in decimal. Default is NULL (no intervention). If not NULL, they
#'   must be provided for each quarter/month of the year.
#' @param conversion type of consistency between the annual benchmarks and the
#'   infra-annual indicators.
#' @import data.table rjd3toolkit rjd3sts
#' @return a list
#' @export
#' @examples
#' Y<-rjd3toolkit::aggregate(rjd3toolkit::retail$RetailSalesTotal, 1)
#' x<-rjd3toolkit::aggregate(rjd3toolkit::retail$FoodAndBeverageStores, 4)
#'
#' outliers<-c(2008.75,2009.00)
#' outliers.intensity<-c(10,5)
#' manual_disagBI<-matrix(cbind(c(2008.00,2008.25,2008.50,2008.75),
#'                              c(7.246695,7.246695,7.246695,6.0)),
#'                        ncol=2,
#'                        dimnames=list(NULL,c("period","bi_ratio")))
#' rslt<-mbdenton(x,Y,outliers,outliers.intensity,manual_disagBI,conversion="Sum")
#'
mbdenton <- function(indicator,
                     benchmark,
                     outliers = NULL,
                     outliers.intensity = 10,
                     manual_disagBI = NULL,
                     conversion = c("Sum", "Average")){

  if (is.matrix(benchmark)){
    if (ncol(benchmark)>1) stop("Mutli-processing not allowed. You can use function multiTD for that.")
    else benchmark_name<-colnames(benchmark)
  } else benchmark_name<-NULL

  # prepare data
  freq<-frequency(indicator)
  yc<-matrix(nrow=freq*length(benchmark), ncol=1)
  yc[freq*(seq_along(benchmark)),1]<-benchmark
  xt<-ts(indicator, frequency = freq, start = start(benchmark), end = c(end(benchmark)[1], freq))
  time_vect<-as.vector(time(indicator))

  # fill yc with manual bi
  if (!is.null(manual_disagBI)){
    if (nrow(manual_disagBI)%%freq != 0) stop("The fixed disaggregated BI ratio provided do not cover the entire year.")
    mbi<-data.table(manual_disagBI)
    mbi[, year:=as.numeric(substr(period,1,4))]
    ym<-unique(mbi$year)
    xt_ym <- unlist(lapply(ym, function(y) as.numeric(window(xt, start = c(y,1), end = c(y, freq)))))
    yT_ym <- unlist(lapply(ym, function(y) as.numeric(window(benchmark, start = c(y,1), end = c(y, 1)))))
    mbi[, xt := xt_ym]
    mbi[, yt := bi_ratio * xt]

    discrepancies_T <- cbind(mbi[, .(yT_ym_manual = sum(yt)), by = "year"], yT_ym)
    discrepancies_T <- discrepancies_T[, discrepancy := yT_ym - yT_ym_manual]
    if (abs(sum(discrepancies_T$discrepancy)) > 0){
      mbi<-merge(mbi, discrepancies_T, by = "year")
      mbi[, w := yt / yT_ym_manual]
      mbi[, yt := yt + (discrepancy*w)]
      discrepancies_T <- discrepancies_T[, discrepancy_perc := discrepancy / yT_ym]
      if (max(abs(discrepancies_T$discrepancy_perc)) > 0.001){
        warning("Significant discrepancy was found using the user-defined disaggregated bi ratios. This was corrected automatically prorata.", call. = FALSE)
      }
    }
    yc_manual<-mbi[, .(period, yc = cumsum(yt)), by = "year"]
    n<-match(yc_manual$period, as.character(time_vect))
    yc[n]<-yc_manual$yc
  }

  # adapt yc when conversion = 'Average'
  if (conversion == "Average"){
    yc <- yc*freq
  }

  # define the vector of relative standard errors
  stderr_beta <- rep(1,length(indicator)) # 1 by default

  if (!is.null(outliers)){
    n <- match(outliers, as.character(time_vect)) - 1 # -1 given recursion
    if (length(outliers.intensity) == 0) outliers.intensity <- 10
    stderr_beta[n] <- outliers.intensity
  }

  # model
  vreg<-rjd3sts::var_reg("x", xt, stderr_beta, scale=1, fixed=TRUE)
  vc<-rjd3sts::cumul("c", vreg, freq)
  vmodel<-rjd3sts::model()
  rjd3sts::add(vmodel, vc)
  veq<-rjd3sts::equation("eq")
  rjd3sts::add_equation(veq, "c")
  rjd3sts::add(vmodel, veq)
  vres<-rjd3sts::estimate(vmodel, yc, marginal = TRUE)

  # results "in-sample"
  vbeta<-rjd3toolkit::result(vres, "ssf.smoothing.array(1)")
  vbeta_ts<-ts(vbeta, frequency = freq, start = start(xt), end = end(xt))
  vbeta_var<-rjd3toolkit::result(vres, "ssf.smoothing.varray(1)")
  vbeta_var[vbeta_var < 0] <- 0
  ll <- rjd3toolkit::result(vres, "likelihood.ll")

  # extrapolation
  ni<-length(vbeta)
  ne<-length(indicator)-ni
  vbeta_ext<-c(vbeta,rep(vbeta[ni],ne))
  vbeta_ext_ts<-ts(vbeta_ext, frequency = freq, start = start(indicator), end = end(indicator))

  pT<-vbeta_var[ni]
  QT<-rjd3toolkit::result(vres, "scalingfactor")*stderr_beta[ni]
  var_out<-vector(mode = "numeric",length = ne)
  pTi<-pT
  for(i in 1:ne){
    pTi<-pTi+QT
    var_out[i]<-pTi
  }
  vbetaSD_ext_ts <- ts(sqrt(c(vbeta_var, var_out)), frequency = freq, start = start(indicator), end = end(indicator))

  vdisag_ext_ts <- indicator*vbeta_ext_ts

  # Output
  out <- list()
  out$disag <- vdisag_ext_ts
  out$beta <- vbeta_ext_ts
  out$betaSD <- vbetaSD_ext_ts
  out$ll <- ll

  return(out)
}
