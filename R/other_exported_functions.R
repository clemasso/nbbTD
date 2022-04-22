
#' Calculate annual BI ratio
#'
#' @param s ts or mts object annual benchmark(s)
#' @param i ts or mts object infra-annual indicator(s)
#' @param conversion a character object indicating the type of conversion
#' @return ts or mts object annual BI ratio(s)
#' @export
#' @examples
#'
#' data(nbb_data)
#' B1G_Y_data <- nbb_data$B1G_Y_data
#' TURN_Q_data <- nbb_data$TURN_Q_data
#'
#' # convert data to ts object
#' B1G_Y_data_ts <- ts(B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#' TURN_Q_data_ts <- ts(TURN_Q_data[,-1], frequency = 4, start = c(2009,1))
#'
#' CalcBIratio(B1G_Y_data_ts, TURN_Q_data_ts) #colnames of \code{s} is used for the output
#'
CalcBIratio <- function(s, i, conversion = c("Sum", "Average")){

  conversion <- match.arg(conversion)

  mts <- FALSE

  if(is.mts(s) & is.mts(i)){
    mts <- TRUE
    if (nrow(s) > nrow(i)/frequency(i)) stop("ERROR: indicator series too short compared with benchmark series")
  } else if((is.mts(s) & !is.mts(i)) | (!is.mts(s) & is.mts(i))) {
    stop("Benchmark series is an mts object and not the indicator series or conversely")
  } else if(is.ts(s) & is.ts(i)){
    if (length(s) > length(i)/frequency(i)) stop("ERROR: indicator series too short compared with benchmark series")
  } else{
    stop("Benchmark series and indicator series must be a ts or mts object")
  }

  if(start(s)[1] != start(i)[1] | start(s)[2] != start(i)[2]) stop("ERROR: indicator series does not start at the same time as the benchmark series")

  # aggregate indicator on annual basis
  if(conversion == "Sum"){
    iY_ts <- aggregate.ts(i, nfrequency = 1,  FUN = sum)
  } else if (conversion == "Average"){
    iY_ts <- aggregate.ts(i, nfrequency = 1,  FUN = mean)
  }

  iY_ts <- window(iY_ts, start = start(s), end = end(s))

  # calculate benchmark to indicator ratio
  bi_ts <- s / iY_ts
  colnames(bi_ts) <- colnames(s)

  return(bi_ts)
}

#' Validation and formatting of outliers in infra-annual BI ratios
#'
#' Validate and format initial data.table which include the infra-annual BI
#' outliers (or BI shifts) in a way that can be handled by the function 'multiTD',
#' 'multiTDDenton' and multiTDCLvar'. See help of the function 'setConfig_default'
#' for more info about the structure of the input.
#'
#' @param bi_q_outliers a data.frame or data.table object with 4 columns:
#'                     'series_name', 'year', quarter_month' and 'intensity'
#'                     containing manual outliers (and their intensity, set to
#'                     10 by default).
#' @param benchmarks_series_names a character vector containing the valid
#'                                the series names of the benchmarks series
#' @param freq a numeric object: 4 for quarterly data, 12 for monthly data
#' @import data.table
#' @return data.table object
#' @export
#' @examples
#' # see ?setConfig_default for more details about the structure of the original input
#' bi_q_outliers <- data.frame(series_name  = "CE",
#'                             year = 2020,
#'                             quarter_month = 2,
#'                             intensity = 10) # keep intensity=10 by default
#' bi_q_outliers_form <- ValidateAndFormatBIqOutliers(bi_q_outliers, c("CE", "XXX", "YYY"), 4)
#'
ValidateAndFormatBIqOutliers <- function(bi_q_outliers, benchmarks_series_names, freq){

  setDT(bi_q_outliers)

  # Validation
  bi_q_outliers <- check_series_name(bi_q_outliers, benchmarks_series_names, "bi_q_outliers")

  # Formatting
  bi_q_outliers_form <- copy(bi_q_outliers)

  if(nrow(bi_q_outliers) > 0){
    bi_q_outliers_form[, period := decimal_period(year, quarter_month, freq), by = seq_len(nrow(bi_q_outliers))]
    bi_q_outliers_form[, `:=`(year = NULL, quarter_month = NULL)]
  } else{
    bi_q_outliers_form <- NULL
  }

  return(bi_q_outliers_form)
}


#' Validation and formatting of manual infra-annual BI ratios
#'
#' Validate and format initial data.table which include the manual infra-annual
#' BI ratios in a way that can be handled by the function 'multiTD', 'multiTDDenton'
#' and multiTDCLvar'. See help of the function 'set_multiTDConfig' for more info
#' about the structure of the input.
#'
#' @param bi_q_manual a data.frame or a data.table object with 14 columns: '
#'                    series_name', 'year', 'q1_m1', ..., 'q4_m4', 'm5', ...,
#'                    'm12' containing anual BI ratios on an infra-annual basis.
#' @param benchmarks_series_names a character vector containing the valid
#'                                the series names of the benchmarks series
#' @param freq a numeric object: 4 for quarterly data, 12 for monthly data
#' @import data.table
#' @return data.table object
#' @export
#' @examples
#' # see ?setConfig_default for more details about the structure of the original input
#' bi_q_manual <- data.frame(series_name  = "CE",
#'                            year = 2020,
#'                            q1_m1 = 24.2,
#'                            q2_m2 = 23.5,
#'                            q3_m3 = 24.2,
#'                            q4_m4 = 24.2)
#' bi_q_manual_form <- ValidateAndFormatBIqManual(bi_q_manual, c("CE", "XXX", "YYY"), 4)
#'
ValidateAndFormatBIqManual <- function(bi_q_manual, benchmarks_series_names, freq){

  setDT(bi_q_manual)

  # Validation
  bi_q_manual <- check_series_name(bi_q_manual, benchmarks_series_names, "bi_q_manual")

  # Formatting
  bi_q_manual_form <- copy(bi_q_manual)

  if(nrow(bi_q_manual) > 0){
    if(freq==4) bi_q_manual <- bi_q_manual[,1:6] # remove useless columns when quarterly data
    bi_q_manual_form <- melt(bi_q_manual, id.vars = c("series_name", "year"), variable.name = "quarter_month", value.name = "bi_ratio")
    bi_q_manual_form[, quarter_month := as.numeric(substring(quarter_month, nchar(as.character(quarter_month)),  nchar(as.character(quarter_month))))]
    setorder(bi_q_manual_form, series_name, year, quarter_month)
    bi_q_manual_form[, period := decimal_period(year, quarter_month, freq), by = seq_len(nrow(bi_q_manual_form))]
    bi_q_manual_form[, `:=`(year = NULL, quarter_month = NULL)]
  } else{
    bi_q_manual_form <- NULL
  }

  return(bi_q_manual_form)
}


#' Automatic forecast of annual BI ratio's
#'
#' @param bi_ts mts object annual BI ratio's to forecast
#' @param critical.value a numeric object threshold value to use in the
#'                       automatic selection of  the model
#' @param bi_f_manual a data.frame object with 3 columns: 'series_name',
#'                   'f_T1' and 'f_T2' containing manual forecast of annual
#'                    BI ratio for years T+1 and T+2. If the data.frame is
#'                    empty or if f_bi_manual = NULL, the results of the
#'                    automatic selection of forecasting model is used by
#'                    default for each series.
#' @import data.table
#' @return list of 2 elements with primary and alternative forecasts
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data <- nbb_data$B1G_Y_data
#' TURN_Q_data <- nbb_data$TURN_Q_data
#'
#' # convert data to ts object
#' B1G_Y_data_ts <- ts(B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#' TURN_Q_data_ts <- ts(TURN_Q_data[,-1], frequency = 4, start = c(2009,1))
#'
#' # compute annual Bi ration
#' bi_ts <- CalcBIratio(B1G_Y_data_ts, TURN_Q_data_ts)
#'
#' # forecast of annual BI ratio (only used when enhanced=TRUE)
#' data(table_rw)
#' f_bi_ts <- ForecastAnnualBIRatios(bi_ts, critical.value = table_rw[series_length == nrow(bi_ts), q.95])
#'
ForecastAnnualBIRatios <- function(bi_ts, critical.value, bi_f_manual = NULL){

  # Automatic selection of forecasting model

  ## Calculate prediction score for each (normalized) BI ratio
  ## The score is defined as the difference in RMSE of cross-validation errors between the best alternative model and a random walk process
  ## H0: RW process / HA: an alternative model
  bi_norm <- apply(bi_ts, 2, function(x) (x - mean(x))/sd(x) + 100) # normalization with mean=100 (to avoid problems with negative values)
  rmseCV <-  apply(bi_norm, 2, function(x) CalcCVRMSEOfSelectedModels(x))
  f_mod <- calculate_pred_score(rmseCV)

  ## Critical value
  f_mod[, threshold := critical.value]

  ## Selection of model
  f_mod[, mod1 := ifelse(score < threshold | altmod == "", "rw", as.character(altmod))]
  f_mod[, mod2 := ifelse(mod1 == "rw", as.character(altmod), "rw")]

  # Forecast with selected model
  f_mod_1 <- f_mod[,.(series_name, mod1, altmod_wd)]
  f_mod_2 <- f_mod[,.(series_name, mod2, altmod_wd)]
  f_bi_edenton <- sapply(1:ncol(bi_ts),
                         function(x) ForecastSeries(bi_ts[,x], as.character(f_mod_1[x,2]), fh = 2, wd = as.numeric(f_mod_1[x,3])))
  falt_bi_edenton <- sapply(1:ncol(bi_ts),
                            function(x) ForecastSeries(bi_ts[,x], as.character(f_mod_2[x,2]), fh = 2, wd = as.numeric(f_mod_2[x,3])))
  colnames(f_bi_edenton) <- colnames(falt_bi_edenton) <- colnames(bi_ts)

  # Replace automatic forecasts by manual forecast when required (falt takes the value of f in this case)
  if(!is.null(bi_f_manual)){
    f_list <- add_f_manual(f_bi_edenton, falt_bi_edenton, bi_f_manual)
    f_bi_edenton <- f_list[[1]]
    falt_bi_edenton <- f_list[[2]]
  }

  # Output as 'ts' object
  f_bi_ts <- ts(f_bi_edenton, frequency = 1, start = next_period(end(bi_ts), 1))
  falt_bi_ts <- ts(falt_bi_edenton, frequency = 1, start = next_period(end(bi_ts), 1))
  output <- list(f_bi_ts = f_bi_ts, falt_bi_ts = falt_bi_ts)

  return(output)
}


#' Calculate RMSE of cross validation errors for several forecasting models
#'
#' @param s \code{"\link{ts}"} object annual time series
#' @import forecast
#' @return matrix
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data_ts <- ts(nbb_data$B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#'
#' #select one series
#' s_CE <- B1G_Y_data_ts[,1]
#'
#' CalcCVRMSEOfSelectedModels(s_CE)
#'
CalcCVRMSEOfSelectedModels <- function(s){

  if(length(s) >= 13){
    rmse_h0 <- CalcCVRMSE(s, model="rw", min.origin = 10)
    rmse_mean5 <- CalcCVRMSE(s, model="mean", wd = 5, min.origin = 10)
    rmse_gmeang5 <- CalcCVRMSE(s, model="gmeang", wd = 5, min.origin = 10)
    rmse_tramo <- CalcCVRMSE(s, model="tramo", min.origin = 10)
    rmse_all <- c(rw = rmse_h0, mean5 = rmse_mean5, gmeang5 = rmse_gmeang5, tramo = rmse_tramo)

  } else if (length(s) >= 8){
    rmse_h0 <- CalcCVRMSE(s, model="rw", min.origin = 5)
    rmse_mean5 <- CalcCVRMSE(s, model="mean", wd = 5, min.origin = 5)
    rmse_gmeang5 <- CalcCVRMSE(s, model="gmeang", wd = 5, min.origin = 5)
    rmse_all <- c(rw = rmse_h0, mean5 = rmse_mean5, gmeang5 = rmse_gmeang5)

  } else if (length(s) >= 5){
    rmse_h0 <- CalcCVRMSE(s, model="rw", min.origin = 3)
    rmse_mean3 <- CalcCVRMSE(s, model="mean", wd = 3, min.origin = 3)
    rmse_gmeang3 <- CalcCVRMSE(s, model="gmeang", wd = 3, min.origin = 3)
    rmse_all <- c(rw = rmse_h0, meanshort = rmse_mean3, gmeang3 = rmse_gmeang3)

  } else {
    rmse_all <- NA
  }

  return(rmse_all)
}

#' Calculate RMSE of cross-validation errors for a selected method
#'
#' @param s \code{"\link{ts}"} object
#' @param model forecasting model. 'rw' = random walk (naive) method, 'mean' = average method over a rolling window, ...
#' @param fh forecast horizon
#' @param wd size of the rolling window for the functions 'mean' and 'gmeang'
#' @param min.origin minimum number of periods considered to start cross-validation process. Note that TRAMO needs a minimum of 10 periods.
#' @import forecast
#' @return a vector with RMSE of cross-validation errors for the choosen model
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data_ts <- ts(nbb_data$B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#'
#' #select one series
#' s_CE <- B1G_Y_data_ts[,1]
#'
#' CalcCVRMSE(s_CE, model = "holtd")
#'
CalcCVRMSE <- function(s, model = c("rw", "mean", "gmeang", "tramo", "holtd", "regl", "arima"), fh = 1, wd = 5, min.origin = 10){

  model=match.arg(model)

  # Calculate cross-validation errors according to the selected model
  if (model=="rw") {
    cve <- tsCV(s, rwf, h=fh, initial = min.origin-1)
  } else if (model=="mean") {
    cve <- tsCV(s, meanf, h=fh, initial = min.origin-wd, window = wd)
  } else if (model=="gmeang") {
    cve <- calc_cv_gmeang (s, h=fh, mw.size = wd, min.origin = min.origin)
  } else if (model=="tramo") {
    cve <- calc_cv_tramo(s, h=fh, min.origin = min.origin)
  } else if (model=="holtd") {
    holtdf <- function(x, h){holt(x, damped = TRUE, h = fh)}
    cve <- tsCV(s, holtdf, h=fh, initial = min.origin-1)
  } else if (model=="regl") {
    regf <- function(x, h){forecast(tslm(x ~ trend), h = fh)}
    cve <- tsCV(s, regf, h=fh, initial = min.origin-1)
  } else if (model=="arima") {
    arimaf <- function(x, h){forecast(auto.arima(x), h = fh)}
    cve <- tsCV(s, arimaf, h=fh, initial = min.origin-1)

  } else stop("Model not supported")

  if(all(is.na(cve))) warning(paste("No CV errors could be computed with the model", model))

  # Calculate RMSE of cross-validation errors
  rmse_cve <- sqrt(mean(cve^2, na.rm=TRUE))

  return(rmse_cve)
}

#' Forecast annual time series according to a selected method
#'
#' @param s \code{"\link{ts}"} object
#' @param model model used to forecast. 'rw' = random walk (naive) method, 'mean' = average method over a rolling window, ...
#' @param fh forecast horizon
#' @param wd size of the rolling window for the functions 'mean' and 'gmeang' (ignored when other method)
#' @import forecast
#' @return vector with forecasts
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data_ts <- ts(nbb_data$B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#'
#' #select one series
#' s_CE <- B1G_Y_data_ts[,1]
#'
#' ForecastSeries(s_CE, model = "holtd")
#'
ForecastSeries <- function(s, model = c("rw", "mean", "gmeang", "tramo",  "holtd", "arima", "regl"), fh = 2, wd = 5){

  model=match.arg(model)

  if (model == "rw") {
    m <- rwf(s, fh)
    f <- as.vector(m$mean)
  } else if (model=="mean") {
    s_l <- s[(length(s)-(wd-1)):length(s)]
    m <- meanf(s_l, fh)
    f <- as.vector(m$mean)
  } else if (model=="gmeang") {
    s_l <- s[(length(s)-wd):length(s)]
    f <- gmeangf(ts(s_l), fh)
  } else if (model=="holtd") {
    m <- holt(s, damped = TRUE, h = fh)
    f <- as.vector(m$mean)
  } else if (model=="arima") {
    m <- forecast(auto.arima(s), h = fh)
    f <- as.vector(m$mean)
  } else if (model=="regl") {
    m <- forecast(tslm(s ~ trend), h = fh)
    f <- as.vector(m$mean)
  } else stop("Model not supported")

  if(any(is.na(f))) warning(paste("Some series could not be forecasted", model))

  return(f)
}


#' Extend benchmarks series of one year
#'
#' @param benchmarks_ts set of benchmarks time series that will be disaggregated.
#'                      It must be a mts object.
#' @param indicators_ts set of corresponding indicators time series used for
#'                      temporal disaggregation. It must be a mts
#'                      object. If no indicator exists, the indicator series
#'                      should be defined as NA or any constant so that the
#'                      number of columns is identical to this of the benchmarks.
#'                      Name and order of the columns must be identical to those
#'                      of the benchmarks too.
#' @param f_bi_ts set of forecasts to use for extrapolation when enhanced = TRUE.
#'                They can be generated by running the function
#'                'ForecastAnnualBIRatios'.
#' @param conversion type of consistency between the annual benchmarks and the
#'                   infra-annual indicators
#' @return ts object with extended benchmarks_ts
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data <- nbb_data$B1G_Y_data
#' TURN_Q_data <- nbb_data$TURN_Q_data
#' colnames(B1G_Y_data) <- colnames(TURN_Q_data) <- c("DATE", "CE", "FF", "HH") # mandatory to match colnames
#'
#' # convert data to ts object
#' B1G_Y_data_ts <- ts(B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#' TURN_Q_data_ts <- ts(TURN_Q_data[,-1], frequency = 4, start = c(2009,1))
#'
#' # forecast of annual BI ratio (only used when enhanced=TRUE)
#' bi_ts <- CalcBIratio(B1G_Y_data_ts, TURN_Q_data_ts)
#' data(table_rw)
#' f_bi_ts <- ForecastAnnualBIRatios(bi_ts, critical.value = table_rw[series_length == nrow(bi_ts), q.95])$f_bi_ts
#'
#' B1G_Y_data_ts_EXTENDED <- ExtendBenchmarksSeries(B1G_Y_data_ts, TURN_Q_data_ts, f_bi_ts)
#'
ExtendBenchmarksSeries <- function(benchmarks_ts, indicators_ts, f_bi_ts, conversion = c("Sum", "Average")){

  # Check input

  ## Consistency in the series names of benchmarks and indicators
  is_colnames_identical(benchmarks_ts, indicators_ts)
  is_colnames_identical(benchmarks_ts, f_bi_ts)

  ## Treatment of missing values
  indicators_ts <- ReplaceNAColByCst(indicators_ts) # replace NA by a constant when no indicator is provided
  if(any(is.na(indicators_ts)) | any(is.na(benchmarks_ts))) stop("missing observations are not handled")

  ## Conversion value
  conversion <- match.arg(conversion)

  # Peform extension
  T1 <- start(f_bi_ts)[1]
  i_T1_ts <- window(indicators_ts, start = c(T1, 1), end = c(T1, frequency(indicators_ts)))

  if(conversion == "Sum"){
    i_T1_Y_ts <- aggregate.ts(i_T1_ts, nfrequency = 1, FUN = sum)
  } else if (conversion == "Average"){
    i_T1_Y_ts <- aggregate.ts(i_T1_ts, nfrequency = 1, FUN = mean)
  }

  b_T1_ts <- i_T1_Y_ts * f_bi_ts[1,]

  b_ext_ts <- ts(rbind(benchmarks_ts, b_T1_ts), start=start(benchmarks_ts), frequency=1)

  return(b_ext_ts)
}



#' Calculate implicit forecasts of annual BI ratio.
#'
#' While it is defined explicitly when eDenton method is used, this is not
#' the case when other methods are used. This function calculates the implicit
#' forecast of the annual BI ratio's for the full years of extrapolation.
#'
#' @param td_series set of temporal disaggregated series. It must be
#'                  a mts object.
#' @param indicators_ts set of corresponding indicators time series used for
#'                      temporal disaggregation. It must be a mts object.
#' @param start a numeric value which is the first extrapolated year.
#' @param conversion type of consistency between the annual benchmarks and the
#'                   infra-annual indicators
#' @return ts object with implicit forecasts
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data <- nbb_data$B1G_Y_data
#' TURN_Q_data <- nbb_data$TURN_Q_data
#' colnames(B1G_Y_data) <- colnames(TURN_Q_data) <- c("DATE", "CE", "FF", "HH") # mandatory to match colnames
#'
#' # convert data to ts object
#' B1G_Y_data_ts <- ts(B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#' TURN_Q_data_ts <- ts(TURN_Q_data[,-1], frequency = 4, start = c(2009,1))
#'
#' # define models
#' models <- c("chow-lin", "fernandez", "chow-lin")
#'
#' # run td
#' res <- multiTDCLvar(B1G_Y_data_ts, TURN_Q_data_ts, models, conversion = "Sum")
#'
#' CalcImplicitAnnualBIForecast(res$td.series, TURN_Q_data_ts, 2021)
#'
CalcImplicitAnnualBIForecast <- function(td_series, indicators_ts, start, conversion = c("Sum", "Average")) {

  freq <- frequency(td_series)
  conversion <- match.arg(conversion)

  # keep extrapolation period
  q <- window(td_series, start = start)
  i <- window(indicators_ts, start = start)

  # annual aggregation
  if(conversion == "Sum"){
    qY <- aggregate.ts(q, nfrequency = 1, FUN = sum)
    iY <- aggregate.ts(i, nfrequency = 1, FUN = sum)
  } else if (conversion == "Average"){
    qY <- aggregate.ts(q, nfrequency = 1, FUN = mean)
    iY <- aggregate.ts(i, nfrequency = 1, FUN = mean)
  }

  # initialize forecast matrix
  bi_f <- ts(matrix(data = NA, nrow = 2, ncol = ncol(td_series), dimnames = list(NULL, colnames(td_series))), start = start)

  # fill it
  nY <- min(nrow(qY), 2)
  bi_f[1:nY,] <- matrix(qY[1:nY,] / iY[1:nY,], nrow = nY)

  return(bi_f)
}


#' Plot annual BI ratio for a specific series
#'
#' @param bi_ts set of Bi ratio's. It must be a mts object.
#' @param f_bi_ts set of forecasted Bi ratio's. It must be a mts object.
#' @param series_name a character object. Name of the series to plot
#' @param scaled a boolean object. If scaled = TRUE, the BI ratio is scaled on
#'               to facilitate the interpretation of the change from year to year
#' @param \dots further arguments passed to or from other methods.
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data <- nbb_data$B1G_Y_data
#' TURN_Q_data <- nbb_data$TURN_Q_data
#' colnames(B1G_Y_data) <- colnames(TURN_Q_data) <- c("DATE", "CE", "FF", "HH") # mandatory to match colnames
#'
#' # convert data to ts object
#' B1G_Y_data_ts <- ts(B1G_Y_data[,-1], frequency = 1, start = c(2009,1))
#' TURN_Q_data_ts <- ts(TURN_Q_data[,-1], frequency = 4, start = c(2009,1))
#'
#' # forecast of annual BI ratio (only used when enhanced=TRUE)
#' bi_ts <- CalcBIratio(B1G_Y_data_ts, TURN_Q_data_ts)
#' data(table_rw)
#' f_bi_ts <- ForecastAnnualBIRatios(bi_ts, critical.value = table_rw[series_length == nrow(bi_ts), q.95])$f_bi_ts
#'
#' PlotAnnualBIratio(bi_ts, f_bi_ts, series_name = "HH", scaled = TRUE)
#'
PlotAnnualBIratio <- function(bi_ts, f_bi_ts, series_name, scaled = FALSE, ...){

  # define first and end periods
  Y1 <- start(bi_ts)
  YN <- end(bi_ts)

  # select results
  bi_ts <- bi_ts[, series_name]
  f_bi_ts <- f_bi_ts[, series_name]
  nf <- length(f_bi_ts)

  if(scaled){
    bi_scaled_ts <- 100 * bi_ts / mean(bi_ts)
    bi_f_scaled_ts <- 100 * f_bi_ts / mean(bi_ts)
  }

  # plot results
  if(!scaled){
    ts.plot(bi_ts, gpars=list(xlab="", xaxt="n", ylab="", type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "blue", xlim = c(Y1[1], YN[1]+nf)), ylim = c(min(bi_ts, f_bi_ts, na.rm = TRUE), max(bi_ts, f_bi_ts, na.rm = TRUE)), ...)
    lines(f_bi_ts, type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "red")
    axis(1, at = seq(Y1[1], YN[1]+nf, by = 1), las=2)
  } else{
    ts.plot(bi_scaled_ts, gpars=list(xlab="", xaxt="n", ylab="", type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "blue", xlim = c(Y1[1], YN[1]+nf)), ylim = c(min(bi_scaled_ts, bi_f_scaled_ts, na.rm = TRUE), max(bi_scaled_ts, bi_f_scaled_ts, na.rm = TRUE)), ...)
    lines(bi_f_scaled_ts, type = "o", lwd=2, pch=19, cex=1.2, las=2, col = "red")
    axis(1, at = seq(Y1[1], YN[1]+nf, by = 1), las=2)
  }
}






