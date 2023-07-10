# Automatic forecast of annual BI ratio's
# Calculate prediction score for each (normalized) BI ratio
# The score is defined as the difference in RMSE of cross-validation errors between the best alternative model and a random walk process
# H0: RW process / HA: an alternative model
forecast_annual_bi <- function(bi_ts, critical_value){
  
  # normalization with mean=100 (to avoid problems with negative values)
  bi_norm<-(bi_ts - mean(bi_ts))/sd(bi_ts) + 100
  
  # cross-validation rmse
  rmseCV<-CalcCVRMSEOfSelectedModels(bi_norm)
  
  if(!all(is.na(rmseCV))){
    # model selection
    pred_score<-calculate_pred_score(rmseCV)
    sn<-as.numeric(pred_score[["score"]])
    alt_mod<-pred_score[["alt_mod"]]
    alt_wd<-pred_score[["alt_wd"]]
 
    mod1<-ifelse(sn < critical_value | is.na(sn), "rw", alt_mod)
    if(mod1 == "rw"){
      wd_mod1<-NA
      mod2<-as.character(alt_mod)
      wd_mod2<-alt_wd
    }else{
      wd_mod1<-alt_wd
      mod2<-"rw"
      wd_mod2<-NA
    }
    
    # forecast with selected model
    f_mod1<-forecast_series(bi_ts, mod1, fh = 2, wd = wd_mod1)
    f_mod2<-forecast_series(bi_ts, mod2, fh = 2, wd = wd_mod2)
    names(f_mod1)<-names(f_mod2)<-c("Y+1","Y+2")
  }
  return(list(f=f_mod1,falt=f_mod2))
}

# Calculate RMSE of cross validation errors for several forecasting models
CalcCVRMSEOfSelectedModels <- function(s){
  
  if(length(s) >= 13){
    rmse_h0 <- CalcCVRMSE(s, model="rw", min.origin = 10)
    rmse_mean5 <- CalcCVRMSE(s, model="mean", wd = 5, min.origin = 10)
    rmse_gmeang5 <- CalcCVRMSE(s, model="gmeang", wd = 5, min.origin = 10)
    #rmse_tramo <- CalcCVRMSE(s, model="tramo", min.origin = 10)
    rmse_all <- c(rw = rmse_h0, mean5 = rmse_mean5, gmeang5 = rmse_gmeang5)#, tramo = rmse_tramo)
    
  } else if (length(s) >= 8){
    rmse_h0 <- CalcCVRMSE(s, model="rw", min.origin = 5)
    rmse_mean5 <- CalcCVRMSE(s, model="mean", wd = 5, min.origin = 5)
    rmse_gmeang5 <- CalcCVRMSE(s, model="gmeang", wd = 5, min.origin = 5)
    rmse_all <- c(rw = rmse_h0, mean5 = rmse_mean5, gmeang5 = rmse_gmeang5)
    
  } else if (length(s) >= 5){
    rmse_h0 <- CalcCVRMSE(s, model="rw", min.origin = 3)
    rmse_mean3 <- CalcCVRMSE(s, model="mean", wd = 3, min.origin = 3)
    rmse_gmeang3 <- CalcCVRMSE(s, model="gmeang", wd = 3, min.origin = 3)
    rmse_all <- c(rw = rmse_h0, mean3 = rmse_mean3, gmeang3 = rmse_gmeang3)
    
  } else {
    rmse_all <- NA
  }
  
  return(rmse_all)
}

# Calculate RMSE of cross-validation errors for a selected method
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

#' Time series cross validation for TRAMO
#'
#' The function computes the cross validation errors for each period when using TRAMO as forecasting method
#'
#' @param s \code{"\link{ts}"} object
#' @param h forecast horizon
#' @param min.origin minimum number of periods considered to start cross-validation process. Note that 'tramo.forecast' only works with nobs >= 10
#' @return cross-validation errors
#'
calc_cv_tramo <- function(s, h, min.origin = 10){
  
  # number of tests
  i_max <- length(s) - min.origin - h + 1
  
  # initialized vector that will contain cross validation errors
  cve <- as.vector(rep(NA, min.origin-1))
  
  # compute cross validation errors
  for (i in i_max:1){
    
    # shorten series
    to <- min.origin + i_max - i
    exclude_from <- to + 1
    s_i <- s[-(exclude_from:length(s))]
    ss_i <- ts(s_i, start = start(s))
    
    # forecast on horizon h
    ssf_i <- as.numeric(tramo_forecast(ss_i, spec = "trfull", nf = h)[,"forecast"])
    
    # forecast error
    ef <- ssf_i - s[length(s) - i + 1]
    
    # fill cve vector
    cve <- c(cve, ef)
  }
  
  return(cve)
}

#' Time series cross validation for the customized forecast method 'geometric mean of growth rates'
#'
#' The function calculates the geometric mean of the series growth rates as a proxy of the trend over
#' an horizon defined by the user. Then it computes cross validation errors for each period
#' (wrt the 'initial' argument)
#'
#' @param s \code{"\link{ts}"} object
#' @param h forecast horizon
#' @param mw.size size of the moving window of growth rates (pm number of growth rates = number of periods - 1). If not defined, consider the maximum size given min.origin.
#' @param min.origin minimum number of periods considered to start cross-validation process
#' @return cross-validation errors
#'
calc_cv_gmeang <- function(s, h, mw.size = NA, min.origin = 5){
  
  # number of tests
  i_max <- length(s) - min.origin - h + 1
  
  # initialized vector that will contain cross validation errors
  cve <- as.vector(rep(NA, min.origin-1))
  
  # compute cross validation errors
  for (i in i_max:1){
    
    # shorten series
    to <- min.origin + i_max - i
    exclude_from <- to + 1
    s_i <- s[-(exclude_from:length(s))]
    ss <- ts(s_i, start = start(s))
    
    if(!is.na(mw.size)){
      s_imw <- s_i[(length(s_i)-mw.size):length(s_i)]
      ss <- ts(s_imw, end = end(ss))
    }
    
    # compute geometric mean of growth rates
    ssg <- ss / lag(ss, k = -1)
    geomean <- prod(ssg)^(1/length(ssg))
    
    # forecast on horizon h
    ssf <- ss[length(ss)] * (geomean ^ h)
    
    # forecast error
    ef <- ssf - s[length(s) - i + 1]
    
    # fill cve vector
    cve <- c(cve, ef)
  }
  
  return(cve)
}

# Forecast of the customized forecast method 'geometric mean of growth rates'
gmeangf <- function(s, H){
  
  # compute geometric mean of growth rates
  sg <- s / lag(s, k = -1)
  geomean <- prod(sg)^(1/length(sg))
  
  # forecast on horizon H
  sf <- vector()
  for (h in 1:H){
    f <- s[length(s)] * (geomean ^ h)
    sf <- c(sf, f)
  }
  
  return(sf)
}


# calculate prediction score of alternative models (HA) compared to RW (H0)
calculate_pred_score <- function(rmseCV){
  
  h0<-rmseCV[names(rmseCV) == "rw"]
  alt_mod<-rmseCV[names(rmseCV) != "rw",drop=F]
  ha<-min(alt_mod)
  ha_mod<-names(alt_mod)[which.min(alt_mod)]
  score<-h0-ha
  alt_wd<-NA
  if(!is.na(score)){
    ls<-substr(ha_mod,nchar(ha_mod),nchar(ha_mod))
    if(!is.na(suppressWarnings(as.numeric(ls)))){
      alt_wd<-as.numeric(ls)
      ha_mod<-substr(ha_mod,1,nchar(ha_mod)-1)
    }
  }
       
  return(list(score=score,alt_mod=ha_mod,alt_wd=alt_wd))
}


#' Forecast annual time series according to a selected method
#'
#' @param s \code{"\link{ts}"} object
#' @param model model used to forecast. 'rw' = random walk (naive) method, 'mean' = average method over a rolling window, ...
#' @param fh forecast horizon
#' @param wd size of the rolling window for the functions 'mean' and 'gmeang' (ignored when other method)
#' @import forecast
#' @return vector with forecasts
forecast_series <- function(s, model = c("rw", "mean", "gmeang", "tramo",  "holtd", "arima", "regl"), fh = 2, wd = 5){
  
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
  } else if (model=="tramo") {
    f <- as.numeric(tramo.forecast(s, spec = "trfull", nf = 2)[,"forecast"])
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


# Extend benchmarks series of one year
extend_benchmark_series <- function(benchmark, indicator, f_biY, conversion = c("Sum", "Average")){
  
  T1_yr<-end(benchmark)[1]+1
  x_T1 <- window(indicator, start = c(T1_yr, 1), end = c(T1_yr, frequency(indicator)))
  
  if(conversion == "Sum"){
    xY_T1 <- as.numeric(aggregate.ts(x_T1, nfrequency = 1, FUN = sum))
  } else if (conversion == "Average"){
    xY_T1 <- as.numeric(aggregate.ts(x_T1, nfrequency = 1, FUN = mean))
  }
  
  y_T1 <- xY_T1 * as.numeric(f_biY)
  y_ext <- ts(c(as.vector(benchmark), y_T1), start=start(benchmark), frequency=1)
  
  return(y_ext)
}

# Extrapolate infra-annual BI ratio
## use of simplified algorithm from IMF (cfr. paper 'On the extrapolation with the Denton Proportional Benchmarking method' p8)
## note that analytical solution not implemented because it only works when the indicator covers the full year T+1. In that case,
## we just extend the benchmarks ratio of one year.
extrapolate_infra_annual_BI_ratio <- function(s, f_Y, freq, b, i, conversion){
  
  if(freq == 4){
    ## Extract last quarterly BI ratio and compute eta
    l <- length(s)
    s_n_2 <- s[l-2]
    s_n_1 <- s[l-1]
    s_n <- s[l]
    eta <- (s_n - f_Y)/3
    
    ### Compute modified quarterly BI ratio from N-2 to N+4
    s_n_2_m <- s_n_2 + eta/4
    s_n_1_m <- s_n_1 + eta/4
    s_n_m <- s_n - eta/2
    s_n1_m <- s_n_m - eta
    s_n2_m <- s_n1_m - eta
    s_n3_m <- s_n2_m - eta
    s_n4_m <- s_n3_m - eta
    
    ### Benchmark approximation for year T
    year_T <- end(s)[1]
    i_T <- window(i, start = c(year_T, 1), end = c(year_T, freq))
    s_m_T <- ts(c(s[l-3], s_n_2_m, s_n_1_m, s_n_m), frequency = freq, start = c(year_T, 1))
    td_m_T <- s_m_T * i_T
    td_m_T_Y <- if(conversion == "Sum") as.vector(aggregate(td_m_T)) else as.vector(mean(td_m_T))
    diff_T <- as.vector(window(b, start = year_T)) - td_m_T_Y 
    diff_T_vec <-rep(diff_T,freq) 
    dist_T_vec <- if(conversion == "Sum") c(0,0.25,0.25,0.5) else freq*c(0,0.25,0.25,0.5)
    td_m_T_bench <- td_m_T + diff_T_vec*dist_T_vec
    s_m_T_bench <- td_m_T_bench / i_T
    
    ### Adjust approximation for year T+1 consequently to preserve QoQ
    ratio_last <- s_m_T_bench[freq]/s_m_T[freq]
    ratio_last_vec <- rep(ratio_last,freq)
    s_m_T1_adj <- c(s_n1_m, s_n2_m, s_n3_m, s_n4_m) * ratio_last_vec
    
    ### Quarterly BI ratio incl. extrapolation for year T+1 (+ modification for year T)
    s_0 <- window(s, end = c(year_T - 1, freq))
    sf <- ts(c(s_0, s_m_T_bench, s_m_T1_adj), frequency = freq, start = start(s))
    
  } else if(freq == 12){
    ## Extract last monthly BI ratio and compute eta
    l <- length(s)
    s_n_8 <- s[l-8]
    s_n_7 <- s[l-7]
    s_n_6 <- s[l-6]
    s_n_5 <- s[l-5]
    s_n_4 <- s[l-4]
    s_n_3 <- s[l-3]
    s_n_2 <- s[l-2]
    s_n_1 <- s[l-1]
    s_n <- s[l]
    eta <- (s_n - f_Y)/3
    
    ### Compute modified monthly BI ratio from N-8 to N+12
    s_n_8_m <- s_n_8 + eta/12
    s_n_7_m <- s_n_7 + eta/12
    s_n_6_m <- s_n_6 + eta/12
    s_n_5_m <- s_n_5 + eta/12
    s_n_4_m <- s_n_4 + eta/12
    s_n_3_m <- s_n_3 + eta/12
    s_n_2_m <- s_n_2 - eta/6
    s_n_1_m <- s_n_1 - eta/6
    s_n_m <- s_n - eta/6
    s_n1_m <- s_n_m - 17*eta/39
    s_n2_m <- s_n1_m - 17*eta/39
    s_n3_m <- s_n2_m - 17*eta/39
    s_n4_m <- s_n3_m - 17*eta/39
    s_n5_m <- s_n4_m - 17*eta/39
    s_n6_m <- s_n5_m - 17*eta/39
    s_n7_m <- s_n6_m - 17*eta/39
    s_n8_m <- s_n7_m - 17*eta/39
    s_n9_m <- s_n8_m - 17*eta/39
    s_n10_m <- s_n9_m - 17*eta/39
    s_n11_m <- s_n10_m - 17*eta/39
    s_n12_m <- s_n11_m - 17*eta/39
    
    ### Benchmark approximation for year T
    year_T <- end(s)[1]
    i_T <- window(i, start = c(year_T, 1), end = c(year_T, freq))
    s_m_T <- ts(c(s[l-11], s[l-10], s[l-9], s_n_8_m, s_n_7_m, s_n_6_m, s_n_5_m, s_n_4_m, s_n_3_m, s_n_2_m, s_n_1_m, s_n_m), frequency = freq, start = c(year_T, 1))
    td_m_T <- s_m_T * i_T
    td_m_T_Y <- if(conversion == "Sum") as.vector(aggregate(td_m_T)) else as.vector(mean(td_m_T))
    diff_T <- as.vector(window(b, start = year_T)) - td_m_T_Y
    diff_T_vec <- rep(diff_T,freq)
    dist_T_vec <- if(conversion == "Sum") c(0,0,0,0.08,0.08,0.08,0.08,0.09,0.09,0.16,0.17,0.17) else freq*c(0,0,0,0.08,0.08,0.08,0.08,0.09,0.09,0.16,0.17,0.17)
    td_m_T_bench <- td_m_T + diff_T_vec*dist_T_vec
    s_m_T_bench <- td_m_T_bench / i_T
    
    ### Adjust approximation for year T+1 consequently to preserve QoQ
    ratio_last <- s_m_T_bench[freq]/s_m_T[freq]
    ratio_last_vec <- rep(ratio_last,freq)
    s_m_T1_adj <- c(s_n1_m, s_n2_m, s_n3_m, s_n4_m, s_n5_m, s_n6_m, s_n7_m, s_n8_m, s_n9_m, s_n10_m, s_n11_m, s_n12_m) * ratio_last_vec
    
    ### Monthly BI ratio incl. extrapolation for year T+1 (+ modification for year T)
    s_0 <- window(s, end = c(year_T - 1, freq))
    sf <- ts(c(s_0, s_m_T_bench, s_m_T1_adj), frequency = freq, start = start(s))
  }
  
  return(sf)
}
