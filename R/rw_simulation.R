
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






