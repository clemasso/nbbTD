#'
#' Multiprocessing NBB-customized temporal disaggregation of time series
#'
#' Perform multi-processing temporal disaggregation of an annual to quarterly
#' or monthly set of time series. The focus is on the (enhanced) Denton PFD
#' method defined in State Space form. Hence, the user can also add outliers
#' (level shift) in the Benchmark-to-Indicator (BI) ratio and import
#' infra-annual BI ratio manually for some periods. When enhanced Denton is
#' used, an automatic procedure exists to select the method for the forecast
#' of the annual BI ratio but the user can also change the forecasts manually
#' if necessary.In addition to Denton PDF method, Chow-Lin method and its
#' variants can also be used. Either the sum or the average consistency between
#' the annual benchmarks and the quarterly or monthly indicators are handled.
#'
#' @param benchmarks set of benchmarks time series that will be disaggregated.
#'                   It can be an object of class mts or data.frame.
#'                   In the second case, the first column should be in a format
#'                   that can be translated into a Date object.
#'                   Only annual benchmarks are handled.
#' @param indicators set of corresponding indicators time series used for
#'                   temporal disaggregation. As for the benchmarks, it can be
#'                   an object of class mts or data.frame. In the second
#'                   case, the first column should be in a format that can be
#'                   translated into a Date object. If no indicator exists, the
#'                   indicator series should be defined as NA or any constant
#'                   so that the number of columns is identical to this of the
#'                   benchmarks. Name and order of the columns must be identical
#'                   to those of the benchmarks too.
#'                   Only quarterly or monthly indicators are handled.
#' @param config an object of the class 'nbb.dsc.td.multiproc.config'.
#'               The object contains a list with the required information to
#'               perform temporal disaggregation of each series.
#' @param conversion type of consistency between the annual benchmarks and the
#'                   infra-annual indicators
#' @param f.score.quantile quantile to consider when choosing between a random
#'                         walk process and another model to forecast the annual
#'                         BI ratio. A lower value of the quantile will increase
#'                         the propensity to select another model and vice-versa.
#' @param simpl.e.T1 a boolean object. By default, when simpl.e.T1 = FALSE, the
#'                   benchmarks series are automatically extended whenever the
#'                   indicators series cover the year T+1. If simpl.e.T1 = TRUE,
#'                   the simple extrapolation is still used. That allows to a
#'                   void revision of the past of the series. Note that this is
#'                   only relevant when the range of the indicators series
#'                   covers exactly the year T+1. If it goes beyond
#'                   (i.e. covering a part of year T+2), the argument is ignored
#'                   and the benchmarks series are extended.
#' @param path.xlsx a character object containing the path of the XLSX file to
#'                  generate with the main results. If the path is en empty
#'                  string, no XLSX file is created.
#' @import data.table openxlsx forecast
#' @import rjd3toolkit rjd3bench rjd3sts rjd3tramoseats
#' @importFrom zoo as.Date.yearmon
#' @importFrom lubridate date_decimal
#' @return an object of class "nbb.dsc.td.multiproc.output"
#' @export
#' @examples
#' data(nbb_data)
#' B1G_Y_data <- nbb_data$B1G_Y_data
#' TURN_Q_data <- nbb_data$TURN_Q_data
#' colnames(B1G_Y_data) <- colnames(TURN_Q_data) <- c("DATE", "CE", "FF", "HH") # mandatory to match colnames
#'
#' data(table_rw) # table of critical values for the automatic selection of the forecasting model of annual BI ratio with 'eDenton'
#'
#' # define config
#' myconfig <- setConfig_default() # you can change any default argument you want. See ?setConfig_default
#' # or alternatively:
#' \dontrun{
#' myconfig <- set_nbbTDConfig_FromXLSX(file = "data/xxx.xlsx")
#' }
#'
#' res <- multiTD(B1G_Y_data, TURN_Q_data, myconfig)
#'
multiTD <- function(benchmarks, indicators, config,
                    conversion = c("Sum", "Average"),
                    f.score.quantile = c("q.95", "q.90", "q.80", "q.99", "q.999"),
                    simpl.e.T1 = FALSE,
                    path.xlsx = ""){

  cl <- match.call()

  # I. Input formatting and validation

  ## Deal with the conversion of the benchmarks and indicators series as ts object
  if(is.ts(benchmarks)){
    benchmarks_ts <- benchmarks
  } else if(is.data.frame(benchmarks)){
    tmp <- benchmarks[1,1]
    if(is.convertible.to.date(tmp)){
      start_yr <- year(tmp)
      benchmarks_ts <- ts(benchmarks[,-1], start = c(start_yr, 1))
    } else stop("First column of benchmarks data frame cannot be converted to a date object")
  } else stop("'benchmarks' should be either a data frame or a ts object")
  if(!ncol(benchmarks_ts) > 0) stop("No benchmarks series found in the input")

  if(is.ts(indicators)){
    indicators_ts <- indicators
    freq <- frequency(indicators_ts)
  } else if(is.data.frame(indicators)){
    tmp_1 <- indicators[1,1]
    tmp_2 <- indicators[2,1]
    if(is.convertible.to.date(tmp_1) & is.convertible.to.date(tmp_2)){
      start_yr <- year(tmp_1)
      if(month(tmp_2) - month(tmp_1) == 1){
        freq <- 12
        start_per <- month(tmp_1)
      } else if (quarter(tmp_2) - quarter(tmp_1) == 1){
        freq <- 4
        start_per <- quarter(tmp_1)
      } else{
        stop("only monthly or quarterly indicators are allowed")
      }
      if(start_per != 1) stop("indicator series must start at the beginning of the year")
      indicators_ts <- ts(indicators[,-1], frequency = freq, start = c(start_yr, 1))
    } else stop("First column of indicators data frame cannot be converted to a date object")
  } else stop("'benchmarks' should be either a data frame or a ts object")

  ## Number of columns in benchmarks and indicators
  if(ncol(benchmarks) != ncol(indicators)){
    stop("Number of columns in benchmarks and indicators data frame do not match. Please check your input.")
  }

  ## Treatment of missing values
  indicators_ts <- ReplaceNAColByCst(indicators_ts) # replace NA by a constant when no indicator is provided
  if(any(is.na(indicators_ts)) | any(is.na(benchmarks_ts))) stop("missing observations are not handled")

  ## Time inconsistencies between the benchmarks and indicators
  if(!all(start(benchmarks_ts) == start(indicators_ts))) stop("benchmarks series and indicators do not start at the same time")

  ## Config
  if(!class(config) == "nbb.dsc.td.multiproc.config") stop("'config' should be an object of the customized class 'nbb.dsc.td.multiproc.config'")

  ## Values of 'conversion' and 'f.score.quantile'
  conversion <- match.arg(conversion)
  f.score.quantile <- match.arg(f.score.quantile)


  ## II. TD model of each series
  td_mod_config <- data.table(config$model)
  td_mod_config[, model := tolower(model)]

  if(nrow(td_mod_config[series_name == "_ALL_", ]) > 0){
    td_mod_all <- as.character(td_mod_config[series_name == "_ALL_", ][1,"model"])
    td_mod <- data.table(series_name = colnames(benchmarks_ts), model = td_mod_all)
  } else{
    series_all <- data.table(series_name = colnames(benchmarks_ts))
    td_mod <- merge(series_all, td_mod_config, by = "series_name", all.x = TRUE)
    if(any(is.na(td_mod$model))){
      td_mod$model[is.na(td_mod$model)] <- "edenton"
      warning("The temporal disaggregation model was not defined for all series. 'edenton' is used by default. See 'model' in the output.", call. = FALSE)
    }
    if(any(td_mod$model[!td_mod$model %in% c("denton", "edenton", "chow-lin", "fernandez", "litterman")])){
      td_mod$model[!td_mod$model %in% c("denton", "edenton", "chow-Lin", "fernandez", "litterman")] <- "edenton"
      warning("Some temporal disaggregation models are not defined as 'denton', 'edenton', 'chow-lin', 'fernandez' or 'litterman'. 'edenton' was used by default for those cases. See 'model' in the output.", call. = FALSE)
    }
    td_mod <- td_mod[order(match(td_mod$series_name, series_all$series_name)), ] # re-order data
  }


  ## III. Annual BI ratios and more

  ### Calculate annual BI ratios
  bi_ts <- CalcBIratio(benchmarks_ts, indicators_ts, conversion)

  ### Check fit in growth rate between the benchmarks and the (annualized) indicators and between the annual bi ratios and the (annualized) indicators
  fit_benchmarks <- calc_fit_growth(benchmarks_ts, indicators_ts)
  fit_bi <- calc_fit_growth(bi_ts, indicators_ts)
  warn_fit_bi(fit_bi, td_mod)


  ## IV. Temporal disaggregation

  ### Initialization output
  out <- out.edenton <- out.denton <- out.clvar <- list()
  class(out) <- "nbb.dsc.td.multiproc.output"
  class(out.edenton) <- c(class(out), "nbb.dsc.td.multiproc.edenton.output")
  class(out.denton) <- c(class(out), "nbb.dsc.td.multiproc.denton.output")
  class(out.clvar) <- c(class(out), "nbb.dsc.td.multiproc.clvar.output")

  ### Subset 'td_mod' by model/method
  td_mod_edenton <- td_mod[model == "edenton"]
  td_mod_denton <- td_mod[model == "denton"]
  td_mod_clvar <- td_mod[model %in%  c("chow-lin", "fernandez", "litterman")]

  ### Number of infra-annual periods to extrapolate
  ne <- nrow(indicators_ts) - (nrow(benchmarks_ts) * freq)

  ### Load, check and format manual input
  bi_f_manual <- data.table(config$f_bi_manual)
  bi_q_outliers <- data.table(config$bi_q_outliers)
  bi_q_manual <- data.table(config$bi_q_manual)

  bi_f_manual <- check_series_name(bi_f_manual, colnames(benchmarks_ts), "bi_f_manual") # this check is performed in the function 'ValidateAndFormat...' for other manual inputs

  bi_f_manual <- check_compatibility_model(bi_f_manual, td_mod = td_mod_edenton,
                                           warnmsg = "Forecast of annual BI ratio are only handled with 'eDenton'. The annual BI forecast provided by the user was ignored for this series.")
  bi_q_outliers <- check_compatibility_model(bi_q_outliers, td_mod = rbind(td_mod_edenton, td_mod_denton),
                                             warnmsg = "BI outliers are only handled with (e)Denton method. The outlier(s) provided by the user were ignored for this series.")
  bi_q_manual <- check_compatibility_model(bi_q_manual, td_mod = rbind(td_mod_edenton, td_mod_denton),
                                           warnmsg = "Manual changes of quarterly BI ratios are only possible with (e)Denton method. The changes provided by the user were ignored for this series.")

  bi_q_outliers_form <- ValidateAndFormatBIqOutliers(bi_q_outliers, colnames(benchmarks_ts), freq = freq)
  bi_q_manual_form <- ValidateAndFormatBIqManual(bi_q_manual, colnames(benchmarks_ts), freq = freq)


  ### IV.1. Enhanced denton PFD method

  if(nrow(td_mod_edenton) > 0){

    ##### Forecast annual BI ratio

    ###### Input
    bi_edenton_ts <- bi_ts[, td_mod_edenton$series_name, drop=FALSE]

    ###### Critical value from 'table_rw'
    nobs_char <- nrow(bi_edenton_ts)
    if(nobs_char > 100){
      nobs_char <- 100
      warning("series length > 100 years. Critical value used for the automatic selection of the model for the forecast of the annual Bi ratio is this considering the maximum of 100 years. This could be easily modified upon request.")
    }
    q_95 <- table_rw[series_length == nobs_char, get(f.score.quantile)]

    ###### Forecasts
    f_list <- ForecastAnnualBIRatios(bi_edenton_ts, q_95, bi_f_manual)
    f_bi_edenton_ts <- f_list$f_bi_ts
    falt_bi_edenton_ts <- f_list$falt_bi_ts

    ##### Processing
    benchmarks_edenton_ts <- benchmarks_ts[, td_mod_edenton$series_name, drop=FALSE]
    indicators_edenton_ts <- indicators_ts[, td_mod_edenton$series_name, drop=FALSE]

    if(ne < freq | (ne == freq & simpl.e.T1)){
      ##### when the benchmarks series are not extended
      out.denton <- multiTDDenton(benchmarks_edenton_ts, indicators_edenton_ts, enhanced = TRUE, f_bi_edenton_ts, bi_q_outliers_form, bi_q_manual_form, conversion)

    } else if((ne > freq & ne < freq*2) | (ne == freq & !simpl.e.T1)){
      ##### when the benchmarks series are extended of one year
      benchmarks_edenton_ext_ts <- ExtendBenchmarksSeries(benchmarks_edenton_ts, indicators_edenton_ts, f_bi_edenton_ts, conversion)
      out.edenton <- multiTDDenton(benchmarks_edenton_ext_ts, indicators_edenton_ts, enhanced = TRUE, f_bi_edenton_ts, bi_q_outliers_form, bi_q_manual_form, conversion)

    } else{
      stop("Forecasted horizon of two years or more are not supported!")
    }

  }

  ### IV.2. Denton PFD method

  if(nrow(td_mod_denton) > 0){

    #### Select input
    benchmarks_denton_ts <- benchmarks_ts[, td_mod_denton$series_name, drop=FALSE]
    indicators_denton_ts <- indicators_ts[, td_mod_denton$series_name, drop=FALSE]

    #### Processing
    out.denton <- multiTDDenton(benchmarks_denton_ts, indicators_denton_ts, enhanced = FALSE, f_bi_ts = NULL, bi_q_outliers_form, bi_q_manual_form, conversion)
  }

  ### IV.3. Chow-Lin variants

  if(nrow(td_mod_clvar) > 0){

    #### Select input
    benchmarks_clvar_ts <- benchmarks_ts[, td_mod_clvar$series_name, drop=FALSE]
    indicators_clvar_ts <- indicators_ts[, td_mod_clvar$series_name, drop=FALSE]

    #### Process series
    out.clvar <- multiTDCLvar(benchmarks_clvar_ts, indicators_clvar_ts, td_mod_clvar$model, conversion)
  }


  ## V. Global results

  ### Benchmarks and indicators
  benchmarks <- benchmarks_ts
  indicators <- indicators_ts

  ### TD series and infra-annual BI ratio
  td_series <- cbind(out.edenton$td.series, out.denton$td.series, out.clvar$td.series)
  bi_q <- cbind(out.edenton$bi.infra, out.denton$bi.infra, out.clvar$bi.infra)

  if(ncol(td_series>1)){
    colnames(td_series) <- c(colnames(out.edenton$td.series), colnames(out.denton$td.series), colnames(out.clvar$td.series))
    colnames(bi_q) <- c(colnames(out.edenton$td.series), colnames(out.denton$td.series), colnames(out.clvar$td.series))
    td_series <- td_series[,colnames(benchmarks_ts)] # keep original order of the series
    bi_q <- bi_q[,colnames(benchmarks_ts)]
  }

  ### Annual BI ratio
  bi <- bi_ts

  ### Forecasts of annual BI ratio

  #### Initialize output
  f_bi_ts <- falt_bi_ts <- ts(matrix(nrow = 2, ncol = ncol(bi_ts), dimnames = list(NULL, colnames(bi_ts))), start = end(bi_ts)[1] + 1)

  #### 'eDenton' explicit forecast
  if(nrow(td_mod_edenton)>0){
    f_bi_ts[, colnames(f_bi_edenton_ts)] <- f_bi_edenton_ts
    falt_bi_ts[, colnames(falt_bi_edenton_ts)] <- falt_bi_edenton_ts
  }

  #### 'Denton' and 'CLvar' implicit forecast (if extrapolation period > one year)
  if(nrow(td_mod_denton)>0 & ne >= freq){
    f_bi_denton_ts <- CalcImplicitAnnualBIForecast(out.denton$td.series, indicators_denton_ts, start = start(f_bi_ts), conversion)
    f_bi_ts[, colnames(f_bi_denton_ts)] <- f_bi_denton_ts
  }

  if(nrow(td_mod_clvar)>0 & ne >= freq){
    f_bi_clvar_ts <- CalcImplicitAnnualBIForecast(out.clvar$td.series, indicators_clvar_ts, start = start(f_bi_ts), conversion)
    f_bi_ts[, colnames(f_bi_clvar_ts)] <- f_bi_clvar_ts
  }

  #### Results
  f_bi <- f_bi_ts
  falt_bi <- falt_bi_ts


  ## VI. Output

  if(!path.xlsx == ""){
    wb <- createWorkbook()
    addWorksheet(wb, "call")
    addWorksheet(wb, "model")
    addWorksheet(wb, "td.series")
    addWorksheet(wb, "bi.infra")
    addWorksheet(wb, "bi.annual")
    addWorksheet(wb, "bi.annual.f")
    addWorksheet(wb, "bi.annual.falt")
    addWorksheet(wb, "overall.fit")
    addWorksheet(wb, "warnings")

    writeData(wb, "call", as.character(cl), startRow = 1, startCol = 1)
    writeData(wb, "model", td_mod, startRow = 1, startCol = 1)
    writeData(wb, "td.series", data.frame(date=as.Date.yearmon(time(td_series)), td_series), startRow = 1, startCol = 1)
    writeData(wb, "bi.infra", data.frame(date=as.Date.yearmon(time(bi_q)), bi_q), startRow = 1, startCol = 1)
    writeData(wb, "bi.annual", data.frame(date=as.Date.yearmon(time(bi)), bi), startRow = 1, startCol = 1)
    writeData(wb, "bi.annual.f", data.frame(date=as.Date.yearmon(time(f_bi)), f_bi), startRow = 1, startCol = 1)
    writeData(wb, "bi.annual.falt", data.frame(date=as.Date.yearmon(time(falt_bi)), falt_bi), startRow = 1, startCol = 1)
    writeData(wb, "overall.fit", fit_benchmarks, startRow = 1, startCol = 1)
    writeData(wb, "warnings", names(warnings()), startRow = 1, startCol = 1)

    saveWorkbook(wb, file = path.xlsx, overwrite = TRUE)
  }

  out$call <- cl
  out$model <- td_mod
  out$Denton <- out.denton
  out$eDenton <- out.edenton
  out$CLvar <- out.clvar
  out$benchmarks <- benchmarks
  out$indicators <- indicators
  out$td.series <- td_series
  out$bi.infra <- bi_q
  out$bi.annual <- bi
  out$bi.annual.f <- f_bi
  out$bi.annual.falt <- falt_bi
  out$overall.fit <- fit_benchmarks
  out$warnings <- warnings()

  return(out)
}



#' Multiprocessing NBB-customized temporal disaggregation of time series with
#' Denton PFD method
#'
#' Perform temporal disaggregation of an annual to quarterly or monthly set of
#' time series using Denton PFD method. The user has the possibility to add
#' outliers (level shift) in the Benchmark-to-Indicator (BI) ratio and to import
#' manual BI ratio on an infra-annual basis. When enhanced = TRUE, enhanced
#' Denton is performed, and the user must provide the forecasted BI ratios
#' ('f_bi_ts'). Those can be generated by first running the function
#' 'ForecastAnnualBIRatios'.
#'
#' Either the sum or the average consistency between the annual benchmarks and
#' the quarterly or monthly indicators are handled.
#'
#' @param benchmarks_ts set of benchmarks time series that will be disaggregated.
#'                      It must be a mts object.
#' @param indicators_ts set of corresponding indicators time series used for
#'                      temporal disaggregation. It must be a mts object.
#'                      If no indicator exists, the indicator series should
#'                      be defined as NA or any constant so that the number
#'                      of columns is identical to this of the benchmarks.
#'                      Name and order of the columns must be identical to those
#'                      of the benchmarks too.
#' @param enhanced a boolean object. If TRUE, enhanced Denton PFD method is
#'                 performed. In such case forecasts 'f_bi_ts' should be
#'                 provided for each series. Default is FALSE.
#' @param f_bi_ts set of forecasts to use for extrapolation when enhanced = TRUE.
#'                They can be generated by running the function
#'                'ForecastAnnualBIRatios'.
#' @param bi_q_outliers_form set of manual outliers formatted using the function
#'                           'ValidateAndFormatBIqOutliers'
#' @param bi_q_manual_form set of manual infra-annual BI ratio's formatted
#'                         using the function 'validateFormatBIqManual'
#' @param conversion type of consistency between the annual benchmarks and the
#'                   infra-annual indicators
#' @import data.table forecast rjd3toolkit rjd3sts rjd3tramoseats
#' @importFrom zoo as.Date.yearmon
#' @importFrom lubridate date_decimal
#' @return an object of class "nbb.dsc.td.multiproc.edenton.output"
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
#' # outliers in infra-annual BI ratios (see ?setConfig_default for more details about the structure of the original input)
#' bi_q_outliers <- data.frame(series_name  = "CE",
#'                             year = 2020,
#'                             quarter_month = 2,
#'                             intensity = 10) # keep intensity=10 by default
#' bi_q_outliers_form <- ValidateAndFormatBIqOutliers(bi_q_outliers, colnames(B1G_Y_data_ts), 4)
#'
#' # define part of the infra-annual BI ratios manually (! manual BI ratio must be filled for each quarter/month of the year(s) !)
#' bi_q_manual <- data.frame(series_name  = "CE",
#'                            year = 2020,
#'                            q1_m1 = 24.2,
#'                            q2_m2 = 23.5,
#'                            q3_m3 = 24.2,
#'                            q4_m4 = 24.2)
#' bi_q_manual_form <- ValidateAndFormatBIqManual(bi_q_manual, colnames(B1G_Y_data_ts), 4)
#'
#' res <- multiTDDenton(B1G_Y_data_ts, TURN_Q_data_ts, enhanced = TRUE, f_bi_ts, bi_q_outliers_form, bi_q_manual_form , conversion = "Sum")
#'
multiTDDenton <- function(benchmarks_ts, indicators_ts, enhanced = FALSE, f_bi_ts = NULL, bi_q_outliers_form = NULL, bi_q_manual_form = NULL, conversion = c("Sum", "Average")){

  freq <- frequency(indicators_ts)

  # Check input

  ## Consistency in the series names of benchmarks and indicators
  is_colnames_identical(benchmarks_ts, indicators_ts)

  if(enhanced){
    is_colnames_identical(benchmarks_ts, f_bi_ts)
  }

  ## Treatment of missing values
  indicators_ts <- ReplaceNAColByCst(indicators_ts) # replace NA by a constant when no indicator is provided
  if(any(is.na(indicators_ts)) | any(is.na(benchmarks_ts))) stop("missing observations are not handled")

  ## Time inconsistencies between the benchmarks and indicators
  if(!all(start(benchmarks_ts) == start(indicators_ts))) stop("benchmarks series and indicators do not start at the same time")

  ## Consistency in arguments
  if(enhanced & is.null(f_bi_ts)) stop("Forecast of annual BI ratio must be specified when enhanced = TRUE")

  ## Manual input
  bi_q_outliers_form <- check_time_range(bi_q_outliers_form, start_yr = start(benchmarks_ts)[1], end_yr = end(benchmarks_ts)[1],
                                         warnmsg = "Outliers (bi shifts) can only be introduced for the years 'in-sample' (inside the time range of the [extended] benchmark). The outliers defined outside this range were ignored.")
  bi_q_manual_form <- check_time_range(bi_q_manual_form, start_yr = start(benchmarks_ts)[1], end_yr = end(benchmarks_ts)[1],
                                       warnmsg = "Manual bi ratios can only be introduced for the years 'in-sample' (inside the time range of the [extended] benchmark). The manual bi ratios defined outside this range were ignored.")

  ## Conversion value
  conversion <- match.arg(conversion)

  # I. Distribution

  ## Part of the indicators 'in-sample'
  indicators_T_ts <- window(indicators_ts, start = start(benchmarks_ts), end = c(end(benchmarks_ts)[1], freq))

  ## Initialization output
  td <- matrix(nrow = nrow(indicators_T_ts), ncol = ncol(indicators_T_ts), dimnames = list(NULL, colnames(benchmarks_ts)))
  td_ts <- ts(td, frequency = freq, start = start(benchmarks_ts))
  bi_ts <- biSD_ts <- biNONM_ts <- biNO_ts <- biNM_ts <- td_ts
  td_e <- matrix(nrow = nrow(indicators_ts), ncol = ncol(indicators_ts), dimnames = list(NULL, colnames(benchmarks_ts)))
  td_e_ts <- ts(td_e, frequency = freq, start = start(benchmarks_ts))
  bi_e_ts <- biNONM_e_ts <- biNO_e_ts <- biNM_e_ts <- td_e_ts

  ## Perform temporal disaggregation individually using customized denton PFD method
  for(i in 1:ncol(benchmarks_ts)){

    ### Select outliers and manual bi ratio if any
    outliers_i <- outliers_intensity_i <- manual_bi_i <- NULL # default (no outlier/manual input)

    if(!is.null(bi_q_outliers_form)){
      bi_q_outliers_form_i <- bi_q_outliers_form[series_name == colnames(benchmarks_ts)[i],]

      if(nrow(bi_q_outliers_form_i) > 0){
        outliers_i <- bi_q_outliers_form_i$period
        outliers_intensity_i <- bi_q_outliers_form_i$intensity
      }
    }

    if(!is.null(bi_q_manual_form)){
      bi_q_manual_form_i <- bi_q_manual_form[series_name == colnames(benchmarks_ts)[i],]

      if(nrow(bi_q_manual_form_i) > 0){
        manual_bi_i <- copy(bi_q_manual_form_i)
      }
    }

    ### Deal with case when ncol = 1
    if(ncol(indicators_ts) > 1){
      indicator_i <- indicators_T_ts[,i]
    } else{
      indicator_i <- indicators_T_ts
    }

    ### Processing
    res_i_ts <- denton_customized(indicator_i, benchmarks_ts[,i], outliers = outliers_i, outliers.intensity = outliers_intensity_i, manual.bi = manual_bi_i, conversion = conversion)

    ### Collect results
    td_i_ts <- res_i_ts$disag
    bi_i_ts <- res_i_ts$beta
    biSD_i_ts <- res_i_ts$betaSD

    if(!is.null(outliers_i) & !is.null(manual_bi_i)){

      #### Get results without outliers or/and manual input for comparison
      resNONM_i_ts <- denton_customized(indicator_i, benchmarks_ts[,i], conversion = conversion, warn = FALSE)
      resNO_i_ts <- denton_customized(indicator_i, benchmarks_ts[,i], manual.bi = manual_bi_i, conversion = conversion, warn = FALSE)
      resNM_i_ts <- denton_customized(indicator_i, benchmarks_ts[,i], outliers = outliers_i, outliers.intensity = outliers_intensity_i, conversion = conversion, warn = FALSE)
      biNONM_i_ts <- resNONM_i_ts$beta
      biNO_i_ts <- resNO_i_ts$beta
      biNM_i_ts <- resNM_i_ts$beta

      #### Check relevance of outliers if any
      ll1 <- resNO_i_ts$ll
      ll2 <- res_i_ts$ll
      test_value <- 2*abs(ll1 - ll2)
      critical_value <- qchisq(p = 0.05, df = 1, lower.tail=FALSE)
      if(test_value < critical_value){warning(paste0("No evidence of superiority of the model with outlier(s) compared with the model without outlier for the series ", colnames(benchmarks_ts)[i]), call. = FALSE)}

    } else if(!is.null(outliers_i)){

      #### Get results without outliers for comparison
      resNO_i_ts <- denton_customized(indicator_i, benchmarks_ts[,i], manual.bi = manual_bi_i, conversion = conversion, warn = FALSE)
      biNO_i_ts <- resNO_i_ts$beta
      biNONM_i_ts <- biNO_i_ts
      biNM_i_ts <- bi_i_ts

      #### Check relevancy of outliers if any
      ll1 <- resNO_i_ts$ll
      ll2 <- res_i_ts$ll
      test_value <- 2*abs(ll1 - ll2)
      critical_value <- qchisq(p = 0.01, df = 1, lower.tail=FALSE)
      if(test_value < critical_value){warning(paste0("No evidence of superiority of the model with outlier(s) compared with the model without outlier for the series ", colnames(benchmarks_ts)[i]), call. = FALSE)}

    } else if(!is.null(manual_bi_i)){

      #### Get results without manual input for comparison
      resNM_i_ts <- denton_customized(indicator_i, benchmarks_ts[,i], outliers = outliers_i, outliers.intensity = outliers_intensity_i, conversion = conversion, warn = FALSE)
      biNM_i_ts <- resNM_i_ts$beta
      biNONM_i_ts <- biNM_i_ts
      biNO_i_ts <- bi_i_ts

    } else{
      biNONM_i_ts <- biNO_i_ts <- biNM_i_ts <- bi_i_ts
    }

    td_ts[,i] <- td_i_ts
    bi_ts[,i] <- bi_i_ts
    biSD_ts[,i] <- biSD_i_ts
    biNONM_ts[,i] <- biNONM_i_ts
    biNO_ts[,i] <- biNO_i_ts
    biNM_ts[,i] <- biNM_i_ts
  }

  ### BI steadiness
  bi_steadiness <- get_perc_mean_in_CI(bi_ts, biSD_ts)


  # II. Extrapolation

  if(enhanced){

    ## Use of simplified algorithm from IMF (cfr. paper 'On the extrapolation with the Denton Proportional Benchmarking method' p8)
    ## Note: analytical solution not implemented because it only works when the indicator covers the full year T+1. In that case,
    ## we prefer extending the benchmarks ratio of one year. See function 'ExtendBenchmarksSeries'.

    ## Restrict f_bi_ts to the year T+1
    f1_bi_ts <- window(f_bi_ts, start = c(end(benchmarks_ts)[1] + 1, 1), end = c(end(benchmarks_ts)[1] + 1, 1))

    ## Extrapolate and forecast of infra-annual series
    bi_ef_ts <- extrapolate_infra_annual_BI_ratio(bi_ts, f1_bi_ts, freq)

    if(!is.null(bi_q_outliers_form) & !is.null(bi_q_manual_form)){
      biNONM_ef_ts <- extrapolate_infra_annual_BI_ratio(biNONM_ts, f1_bi_ts, freq)
      biNO_ef_ts <- extrapolate_infra_annual_BI_ratio(biNO_ts, f1_bi_ts, freq)
      biNM_ef_ts <- extrapolate_infra_annual_BI_ratio(biNM_ts, f1_bi_ts, freq)
    } else if(!is.null(bi_q_outliers_form)){
      biNO_ef_ts <- extrapolate_infra_annual_BI_ratio(biNO_ts, f1_bi_ts, freq)
      biNONM_ef_ts <- biNO_ef_ts
      biNM_ef_ts <- bi_ef_ts
    } else if(!is.null(bi_q_manual_form)){
      biNM_ef_ts <- extrapolate_infra_annual_BI_ratio(biNM_ts, f1_bi_ts, freq)
      biNONM_ef_ts <- biNM_ef_ts
      biNO_ef_ts <- bi_ef_ts
    } else{
      biNONM_ef_ts <- biNO_ef_ts <- biNM_ef_ts <- bi_ef_ts
    }

    ### Extrapolation part
    bi_e_ts <- window(bi_ef_ts, end = end(indicators_ts))
    td_e_ts <- bi_e_ts * indicators_ts
    colnames(td_e_ts) <- colnames(benchmarks_ts)
    biNONM_e_ts <- window(biNONM_ef_ts, end = end(indicators_ts))
    biNO_e_ts <- window(biNO_ef_ts, end = end(indicators_ts))
    biNM_e_ts <- window(biNM_ef_ts, end = end(indicators_ts))

    ### Forecasting part
    if(end(bi_ef_ts)[1] > end(indicators_ts)[1] | end(bi_ef_ts)[2] > end(indicators_ts)[2]){
      f_start <- next_period(end(indicators_ts), freq)
      bi_f_ts <- window(bi_ef_ts, start = f_start)
    } else{
      bi_f_ts <- NULL
    }

  } else{

    ## Extrapolation of infra-annual series
    ne <- nrow(indicators_ts) - nrow(td_ts) # number of periods to extrapolate
    e_bi <- matrix(bi_ts[nrow(bi_ts),], nrow=ne, ncol=ncol(bi_ts), byrow=TRUE)
    e_biNO <- matrix(biNO_ts[nrow(biNO_ts),], nrow=ne, ncol=ncol(biNO_ts), byrow=TRUE)
    e_biNM <- matrix(biNM_ts[nrow(biNM_ts),], nrow=ne, ncol=ncol(biNM_ts), byrow=TRUE)
    e_biNONM <- matrix(biNONM_ts[nrow(biNONM_ts),], nrow=ne, ncol=ncol(biNONM_ts), byrow=TRUE)

    bi_e_ts <- ts(rbind(bi_ts, e_bi), frequency = freq, start = start(indicators_ts), end = end(indicators_ts))
    td_e <- matrix(bi_e_ts * indicators_ts, ncol=ncol(indicators_ts), dimnames = list(NULL, colnames(indicators_ts)))
    td_e_ts <- ts(td_e, frequency = freq, start = start(indicators_ts), end = end(indicators_ts))
    biNO_e_ts <- ts(rbind(biNO_ts, e_biNO), frequency = freq, start = start(indicators_ts), end = end(indicators_ts))
    biNM_e_ts <- ts(rbind(biNM_ts, e_biNM), frequency = freq, start = start(indicators_ts), end = end(indicators_ts))
    biNONM_e_ts <- ts(rbind(biNONM_ts, e_biNONM), frequency = freq, start = start(indicators_ts), end = end(indicators_ts))

    ## Forecasting of infra-annual series
    f_bi <- matrix(bi_ts[nrow(bi_ts),], nrow=freq, ncol=ncol(bi_ts), byrow=TRUE, dimnames = list(NULL, colnames(benchmarks_ts)))
    f_start <- next_period(end(indicators_ts), freq)
    bi_f_ts <- ts(f_bi, frequency = freq, start = f_start)
  }

  # Output
  out <- list()
  out$td.series <- td_e_ts
  out$bi.infra <- bi_e_ts
  out$bi.infra.f <- bi_f_ts
  out$bi.infra.SD <- biSD_ts
  out$bi.infra.NO <- biNO_e_ts
  out$bi.infra.NM <- biNM_e_ts
  out$bi.infra.NONM <- biNONM_e_ts
  out$bi.outliers <- bi_q_outliers_form
  out$bi.manual <- bi_q_manual_form
  out$bi.steadiness <- bi_steadiness

  class(out) <- "nbb.dsc.td.multiproc.output.denton"

  return(out)
}



#' Multiprocessing temporal disaggregation of time series with
#' Chow-Lin method and its variants
#'
#' Perform temporal disaggregation of an annual to quarterly or monthly set of
#' time series using Chow-Lin method or its variants.
#'
#' Either the sum or the average consistency between the annual benchmarks and
#' the quarterly or monthly indicators are handled.
#'
#' @param benchmarks_ts set of benchmarks time series that will be disaggregated.
#'                      It must be a mts object.
#' @param indicators_ts set of corresponding indicators time series used for
#'                      temporal disaggregation. It must be a mts object.
#'                      If no indicator exists, the indicator series should
#'                      be defined as NA or any constant so that the number
#'                      of columns is identical to this of the benchmarks.
#'                      Name and order of the columns must be identical to those
#'                      of the benchmarks too.
#' @param models a character vector with the name of the model to use for each
#'               series in the order of the columns in 'benchmarks_ts'. There
#'               are 3 possibilities of models: 'chow-lin', 'fernandez'
#'               or 'litterman'.
#' @param conversion type of consistency between the annual benchmarks and the
#'                   infra-annual indicators
#' @import data.table rjd3toolkit rjd3bench
#' @return an object of class 'nbb.dsc.td.multiproc.clvar.output'
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
#' res <- multiTDCLvar(B1G_Y_data_ts, TURN_Q_data_ts, models, conversion = "Sum")
#'
multiTDCLvar <- function(benchmarks_ts, indicators_ts, models, conversion = c("Sum", "Average")){

  freq <- frequency(indicators_ts)

  # Check input

  ## Consistency in the series names of benchmarks and indicators
  is_colnames_identical(benchmarks_ts, indicators_ts)

  ## Treatment of missing values
  indicators_ts <- ReplaceNAColByCst(indicators_ts) # replace NA by a constant when no indicator is provided
  if(any(is.na(indicators_ts)) | any(is.na(benchmarks_ts))) stop("missing observations are not handled")

  ## Time inconsistencies between the benchmarks and indicators
  if(!all(start(benchmarks_ts) == start(indicators_ts))) stop("benchmarks series and indicators do not start at the same time")

  ## Models
  models <- tolower(models)
  pos <- match(models, c("chow-lin", "fernandez", "litterman"))
  if(any(is.na(pos))){
    models[which(is.na(pos))] <- "chow-lin"
    warning("Some models defined in 'multiTDCLvar' were not supported or mispelled. Those where replaced by 'chow-lin' by default.", call. = FALSE)
  }

  ## Conversion value
  conversion <- match.arg(conversion)

  # Initialization of intermediate results
  td <- matrix(nrow = nrow(indicators_ts), ncol = ncol(indicators_ts), dimnames = list(NULL, colnames(benchmarks_ts)))
  td_ts <- ts(td, frequency = freq, start = start(indicators_ts))
  res_c <- list()

  for(i in 1:ncol(benchmarks_ts)){

    ## Pick model
    model <- models[i]
    model_shortcut <- ifelse(model == "chow-lin", "Ar1", ifelse(model == "litterman", "RwAr1", "Rw"))

    ## Perform temporal disaggregation
    if(length(unique(indicators_ts[,i])) > 1){
      res_i <- temporaldisaggregation(benchmarks_ts[,i], indicators = list(indicators_ts[,i]), model = model_shortcut, conversion = conversion)
    } else{
      res_i <- temporaldisaggregation(benchmarks_ts[,i], model = model_shortcut, conversion = conversion) # case without indicator
    }

    ## Quick check of the value of rho
    if(model %in% c("chow-lin", "litterman")){
      rho <- res_i$estimation$parameter
      if(rho == 0){
        warning(paste0(model, " was specified as model for the series ", colnames(benchmarks_ts)[i], ". The MLE of rho is 0. Please be aware that this is equivalent to a pure OLS model with potentiel step effects."), call. = FALSE)
      } else if (rho < 0){
        warning(paste0("!!! A negative rho was estimated for the series ", colnames(benchmarks_ts)[i],  " where ", model, " was specified. This is very likely to give unexpected results !!! You should consider defining another model for this series such as Fernandez."), call. = FALSE)
      }
    }

    ## Customize output
    td_i_ts <- res_i$estimation$disagg
    res_c_i <- list(rho = res_i$estimation$parameter, model = res_i$regression$model, cov = res_i$regression$cov, regeffect = res_i$estimation$regeffect, smoothingpart = res_i$estimation$smoothingpart, ll = res_i$likelihood$ll)

    if(!length(unique(indicators_ts[,i])) > 1){
      td_i_ts <- window(td_i_ts, end = end(indicators_ts))
    }

    td_ts[,i] <- td_i_ts
    res_c[[i]] <- res_c_i
  }

  bi_q <- matrix(td_ts / indicators_ts, ncol=ncol(indicators_ts), dimnames = list(NULL, colnames(indicators_ts)))
  bi_q_ts <- ts(bi_q, frequency = freq, start = start(indicators_ts))
  names(res_c) <- colnames(benchmarks_ts)

  # Output
  out <- list(td.series = td_ts, bi.infra = bi_q_ts, details = res_c)
  class(out) <- "nbb.dsc.td.multiproc.output.clvar"

  return(out)
}
