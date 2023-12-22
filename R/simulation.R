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


### Enhanced Denton PFD method: Creating table with critical value for the
### automatic selection of the model for the forecasting of the annual BI ratio
#
# dtable <- matrix(ncol=11, nrow=96)
# dtable[,1] <- 5:100
# colnames(dtable) <- c("series_length","q.001", "q.01", "q.05", "q.10", "q.20", "q.80", "q.90", "q.95", "q.99", "q.999")
#
# for (l in 5:30){
#   print(Sys.time())
#   score_rw_distrib <- simulate_score_distribution_rw(l=l, n=10000)
#
#   q.001 <- quantile(score_rw_distrib, 0.001)
#   q.01 <- quantile(score_rw_distrib, 0.01)
#   q.05 <- quantile(score_rw_distrib, 0.05)
#   q.10 <- quantile(score_rw_distrib, 0.10)
#   q.20 <- quantile(score_rw_distrib, 0.20)
#   q.80 <- quantile(score_rw_distrib, 0.80)
#   q.90 <- quantile(score_rw_distrib, 0.90)
#   q.95 <- quantile(score_rw_distrib, 0.95)
#   q.99 <- quantile(score_rw_distrib, 0.99)
#   q.999 <- quantile(score_rw_distrib, 0.999)
#
#   dtable[l-4,2] <- q.001
#   dtable[l-4,3] <- q.01
#   dtable[l-4,4] <- q.05
#   dtable[l-4,5] <- q.10
#   dtable[l-4,6] <- q.20
#   dtable[l-4,7] <- q.80
#   dtable[l-4,8] <- q.90
#   dtable[l-4,9] <- q.95
#   dtable[l-4,10] <- q.99
#   dtable[l-4,11] <- q.999
# }
#
#
# dtable_spline <-  apply(dtable, 2, function(x) na.spline(x)) # use spline for approximation (used from n > 30)
#
