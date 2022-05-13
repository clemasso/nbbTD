
# Function to check if character argument can be converted to a 'Date' object
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))

# Function to check if column names of two data frames or two matrices are the same and in the same order
is_colnames_identical <- function(t1, t2){
  for (i in 1:length(colnames(t1))){
    if(colnames(t1)[i] != colnames(t2)[i]) {
      t1_name <- deparse(substitute(t1))
      t2_name <- deparse(substitute(t2))
      stop(paste0("colnames of data frame '", t1_name, "' and '", t2_name, "' don't match. Please check your input"))
    }
  }
}

# Function to replace full NA column in a data frame or a ts object by a constant
ReplaceNAColByCst <- function(t){

  f_rep <- function(x){
    if(all(is.na(x))){
      x[is.na(x)] <- 1 #could be any constant
    } else{
      x
    }
    return(as.numeric(x))
  }

  t_clean <- sapply(t, f_rep)
  t_clean <- as.data.frame(t_clean)

  if(is.ts(t)){
    t_clean <- ts(t_clean, frequency = frequency(t), start = start(t))
  }

  return(t_clean)
}

# Function to warn the user when a signficant relationship is found between the Bi ratio and the indicator
warn_fit_bi <- function(fit_bi, td_mod){

  fit_bi_issues <- fit_bi[p_value <  0.01]
  fit_bi_issues <- merge(fit_bi_issues, td_mod, by = "series_name")
  fit_bi_issues <- fit_bi_issues[model != "chow-lin"]

  lapply(fit_bi_issues$series_name, function(x) warning(paste(paste0("Significant relationship was found between the bi ratio and the indicator in series ", x),
                                                              "This could indicate the presence of an outlier or the necessity to use a model including a constant such as Chow-Lin.",
                                                              "Further analysis should be perform to find the reason and adapt the model if necessary.", sep = "\n"), call. = FALSE))
}

# Functions to check series names
check_series_name <- function(dt, valid_series_names, dt_name){

  if(nrow(dt) > 0){
    series_to_handle <- dt$series_name

    for (k in (1:length(series_to_handle))){
      if(!series_to_handle[k] %in% valid_series_names){
        dt <- dt[!series_name == series_to_handle[k]]
        warning(paste0("In ", dt_name, ", series name  ", series_to_handle[k], "do not match in benchmarks series. The manual forecasts for this series were ignored."), call. = FALSE)
      }
    }
  }
  return(dt)
}

# Functions to check if manual input are relevant regarding the TD method
check_compatibility_model <- function(dt, td_mod, warnmsg){

  if(nrow(dt) > 0){
    series_to_handle <- dt$series_name

    for (k in (1:length(series_to_handle))){
      if(!series_to_handle[k] %in% td_mod$series_name){
        dt <- dt[!series_name == series_to_handle[k]]
        warning(paste0(series_to_handle[k], ": ", warnmsg), call. = FALSE)
      }
    }
  }
  return(dt)
}

# Perform linear regression in growth rate between an annual series y and an infra-annual series x and return measures of the fit
calc_fit_growth <- function(y, x){

  # initialize output
  t_stat <- vector(mode = "numeric", length = ncol(y))
  p_value <- vector(mode = "numeric", length = ncol(y))

  # calculate fit
  for(i in 1:ncol(y)){

    ## get data
    y_i <- y[,i]
    x_i <- x[,i]
    if(length(unique(x_i)) == 1) {
      t_stat[i] <- p_value[i] <- NA
      next
    }
    xY_i <- window(aggregate(x_i, nfreq = 1), start = start(y), end = end(y))

    ## calculate growth rates
    yg_i <- y_i / lag(y_i,-1) - 1
    xYg_i <- xY_i / lag(xY_i,-1) - 1

    ## fit linear regression (on growth rates to avoid spurious regression related to non-stationnary time series)
    fit <- tslm(yg_i ~ xYg_i)
    res <- summary(fit)

    ## collect tstat
    t_stat[i] <- round(res$coefficients[2,3], 3)
    p_value[i] <- round(res$coefficients[2,4], 5)
  }

  # output
  output <- data.table(series_name = colnames(y), t_stat = t_stat, p_value = p_value)

  return(output)
}

# Format date in decimal
decimal_period <- function(year, quarter_month, freq){
  as.numeric(time(ts(start = c(year, quarter_month), frequency = freq)))
}

# calculate prediction score of alternative models (HA) compared to RW (H0)
calculate_pred_score <- function(rmseCV){

  rmseCV_dt <- data.table(t(rmseCV))
  alt_mod_names <- colnames(rmseCV_dt)[colnames(rmseCV_dt) != "rw"]

  rmseCV_dt[, h0 := rw]
  rmseCV_dt[, ha := apply(.SD, 1, min), .SDcols = alt_mod_names]
  rmseCV_dt[, score := h0 - ha]
  rmseCV_dt[, altmod := apply(.SD, 1, function(x) alt_mod_names[which.min(x)]), .SDcols = alt_mod_names]
  rmseCV_dt[, series_name := colnames(rmseCV)]

  rmseCV_dt <- rmseCV_dt[, .(series_name, altmod, score)]

  rmseCV_dt[altmod %like% "mean", `:=`(altmod = substr(altmod, 1, nchar(altmod)-1), altmod_wd = as.numeric(substr(altmod, nchar(altmod), nchar(altmod))))]

  return(rmseCV_dt)
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
    ssf_i <- as.numeric(tramo.forecast(ss_i, spec = "trfull", nf = h)[,"forecast"])

    # forecast error
    ef <- ssf_i - s[length(s) - i + 1]

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

# Replace automatic forecasts by manual forecast when required by the user (and define f )
add_f_manual <- function(f, falt, bi_f_manual){

  if(nrow(bi_f_manual) > 0){

    for(i in 1:nrow(bi_f_manual)){
      series_name_i <- as.character(bi_f_manual[i,1])
      new_f_i <- matrix(as.numeric(bi_f_manual[i,2:3]), ncol = 1)
      j <- which(colnames(f)==series_name_i)
      falt[,j] <- f[,j]
      f[,j] <- new_f_i
    }
  }
  return(list(f,falt))
}

# Find next period of a ts object
next_period <- function(period, freq){

  if(period[2] < freq){
    x <- c(period[1], period[2] + 1)
  } else{
    x <- c(period[1] + 1, 1)
  }

  return(x)
}

# Denton PFD method in state space including the possibility to add outliers (BI shifts) and manual infra-annual BI ratio (do not deal with extrapolation)
denton_customized <- function(indicator, benchmark, outliers = NULL, outliers.intensity = 10, manual.bi = NULL, conversion = c("Sum", "Average"), warn = TRUE){

  # Prepare data for sts model
  freq <- frequency(indicator)
  yc<-matrix(nrow=freq*length(benchmark), ncol=1)
  yc[freq*(1:length(benchmark)),1]<- benchmark
  xt <- ts(indicator, frequency = freq, start = start(benchmark), end = c(end(benchmark)[1], freq))
  time_vect <- as.vector(time(indicator))

  # Fill yc when manual bi ratios are given
  if(!is.null(manual.bi)){
    manual_bi <- copy(manual.bi)
    manual_bi[, year := year(date_decimal(manual_bi$period))]

    ym <- unique(manual_bi$year)
    xt_ym <- unlist(lapply(ym, function(y) as.numeric(window(xt, start = c(y,1), end = c(y, freq)))))
    yT_ym <- unlist(lapply(ym, function(y) as.numeric(window(benchmark, start = c(y,1), end = c(y, 1)))))

    manual_bi[, xt := xt_ym]
    manual_bi[, yt := bi_ratio * xt]

    bi_T <- cbind(manual_bi[, .(yT_ym_manual = sum(yt)), by = "year"], yT_ym)
    bi_T <- bi_T[, discrepancy := yT_ym - yT_ym_manual]

    if(abs(sum(bi_T$discrepancy)) > 0){
      manual_bi <- merge(manual_bi, bi_T, by = "year")
      manual_bi[, w := yt / yT_ym_manual]
      manual_bi[, yt := yt + (discrepancy*w)]
      if(warn) warning(paste0("Some discrepancy was found when using the manual infra-annual bi ratios for the series ", as.character(manual_bi[1,series_name]), ". This was corrected automatically prorata."), call. = FALSE)
    }

    yc_manual <- manual_bi[, .(period, yc = cumsum(yt)), by = "year"]
    n <- match(yc_manual$period, as.character(time_vect))
    yc[n] <- yc_manual$yc
  }

  # Adapt yc when conversion = 'Average'
  if(conversion == "Average"){
    yc <- yc*freq
  }

  # Define standard deviation
  stderr_beta <- rep(1,length(indicator))

  if(!is.null(outliers)){
    ## Position of outliers (= higher standard deviation of error)
    n <- match(outliers, as.character(time_vect)) - 1 # -1 because of recursion in sts

    ## Vector of std errors
    if(length(outliers.intensity) == 0) outliers.intensity <- 10
    stderr_beta_NO <- stderr_beta # stderr before including outlier
    stderr_beta[n] <- outliers.intensity
  }

  # Define sts model (denton with standard deviation which may variate) and make estimates
  vreg<-rjd3sts::varreg("x", xt, stderr_beta, scale=1, fixed=T)
  vc<-rjd3sts::cumul("c", vreg, freq)
  vmodel<-rjd3sts::model()
  rjd3sts::add(vmodel, vc)
  veq<-rjd3sts::equation("eq")
  rjd3sts::add.equation(veq, "c")
  rjd3sts::add(vmodel, veq)
  vres<-rjd3sts::estimate(vmodel, yc, marginal = T)

  # Collect results
  vbeta <- rjd3toolkit::result(vres, "ssf.smoothing.array(1)")
  vbeta_ts <- ts(vbeta, frequency = freq, start = start(xt), end = end(xt))
  vbetaSD <-  sqrt(rjd3toolkit::result(vres, "ssf.smoothing.varray(1)"))
  vbetaSD_ts <- ts(vbetaSD, frequency = freq, start = start(xt), end = end(xt))
  vdisag_ts <- xt*vbeta_ts
  ll <- rjd3toolkit::result(vres, "likelihood.ll")

  # Output
  out <- list()
  out$disag <- vdisag_ts
  out$beta <- vbeta_ts
  out$betaSD <- vbetaSD_ts
  out$ll <- ll

  return(out)
}

# Extrapolate infra-annual BI ratio
## use of simplified algorithm from IMF (cfr. paper 'On the extrapolation with the Denton Proportional Benchmarking method' p8)
## note that analytical solution not implemented because it only works when the indicator covers the full year T+1. In that case,
## we could just extend the benchmarks ratio of one year. This is the strategy followed in the main function 'multiTD'
extrapolate_infra_annual_BI_ratio <- function(s, f_Y, freq){

  if(freq == 4){
    ## Extract last quarterly BI ratio and compute eta
    l <- nrow(s)
    s_n_2 <- s[l-2,]
    s_n_1 <- s[l-1,]
    s_n <- s[l,]
    eta <- (s_n - f_Y)/3

    ### Compute modified quarterly BI ratio from N-2 to N+4
    s_n_2_m <- s_n_2 + eta/4
    s_n_1_m <- s_n_1 + eta/4
    s_n_m <- s_n - eta/2
    s_n1_m <- s_n_m - eta
    s_n2_m <- s_n1_m - eta
    s_n3_m <- s_n2_m - eta
    s_n4_m <- s_n3_m - eta

    ### Extrapolate quarterly BI ratio for year T+1 (+ modification of BI ratio for year T)
    sf <- s
    sf[l-2,] <- s_n_2_m
    sf[l-1,] <- s_n_1_m
    sf[l,] <- s_n_m
    sf <- ts(rbind(sf, s_n1_m, s_n2_m, s_n3_m, s_n4_m), frequency = freq, start = start(s))

  } else if(freq == 12){
    ## Extract last monthly BI ratio and compute eta
    l <- nrow(s)
    s_n_8 <- s[l-8,]
    s_n_7 <- s[l-7,]
    s_n_6 <- s[l-6,]
    s_n_5 <- s[l-5,]
    s_n_4 <- s[l-4,]
    s_n_3 <- s[l-3,]
    s_n_2 <- s[l-2,]
    s_n_1 <- s[l-1,]
    s_n <- s[l,]
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

    ### Extrapolate quarterly BI ratio for year T+1 (+ modification of BI ratio for year T)
    sf <- s
    sf[l-8,] <- s_n_8_m
    sf[l-7,] <- s_n_7_m
    sf[l-6,] <- s_n_6_m
    sf[l-5,] <- s_n_5_m
    sf[l-4,] <- s_n_4_m
    sf[l-3,] <- s_n_3_m
    sf[l-2,] <- s_n_2_m
    sf[l-1,] <- s_n_1_m
    sf[l,] <- s_n_m
    sf <- ts(rbind(sf, s_n1_m, s_n2_m, s_n3_m, s_n4_m, s_n5_m, s_n6_m, s_n7_m, s_n8_m, s_n9_m, s_n10_m, s_n11_m, s_n12_m), frequency = freq, start = start(s))
  }

  return(sf)
}

# Check time range
check_time_range <- function(dt, start_yr, end_yr, warnmsg){

  if(any(dt$year < start_yr | dt$year > end_yr)){
    dt <- dt[year >= start_yr & year <= end_yr,]
    warning(warnmsg, call. = FALSE)
  }

  return(dt)
}

# Measure of BI steadiness, i.e. the percentage of BI mean in confidence interval
get_perc_mean_in_CI <- function(bi, biSD){

  bi_mean <- colMeans(bi, 1)
  n <- nrow(bi)
  perc_mean_in_CI <- vector(mode = "numeric", length = ncol(bi))

  for (j in 1:ncol(bi)){

    bi_j <- bi[,j]
    bi_mean_j <- bi_mean[j]
    biSD_j <- biSD[,j]

    c <- 0

    for(i in 1:n){
      lb <- bi_j[i] - 1.96 * biSD_j[i]
      ub <- bi_j[i] + 1.96 * biSD_j[i]

      if(bi_mean_j >= lb & bi_mean_j <= ub){
        c <- c + 1
      }
    }

    perc_mean_in_CI[j] <- c / n
  }

  output <- data.frame(series_name = colnames(bi), percentage_of_BImean_in_CI = perc_mean_in_CI)

  return(output)
}

# Simulate score distribution of random walk process (H0)
simulate_score_distribution_rw <- function(l, n){

  if(l < 5) stop("Length of the series is very short. You should consider a random walk process by default.")
  distrib <- vector()

  # at least 10000 iterations
  for (i in 1:n){

    # Simulate random walk series
    s <- sim_rw(l, x0 = 100) # normalization with mean=100 to avoid problems related to negative values (and values near 0)

    # Calculate RMSE of cross-validation errors
    rmse_all <- CalcCVRMSEOfSelectedModels(s)

    # Calculate score
    rmse_h0 <- rmse_all[names(rmse_all) == "rw"]
    rmse_all_ha <- rmse_all[names(rmse_all) != "rw"]
    rmse_ha <- min(rmse_all_ha)

    rmse_diff <- as.numeric(rmse_h0 - rmse_ha)
    distrib <- c(distrib, rmse_diff)
  }

  return(distrib)
}

# Function to simulate random walk process (adapted from https://bookdown.org/kochiuyu/Technical-Analysis-with-R/simulation.html)
# period (N), initial value (x0), drift (mu), and variance (var)
sim_rw <- function(N, x0, mu = 0, var = 1) {
  z <- cumsum(rnorm(n=N, mean=0, sd=sqrt(var)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}


