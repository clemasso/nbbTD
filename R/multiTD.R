#'
#' Multiprocessing temporal disaggregation of time series
#'
#' Perform multi-processing temporal disaggregation of an annual to quarterly or
#' monthly set of time series. Amongst the possible methods, the model-based
#' proportional first difference (PFD) Denton is defined in State Space form. It
#' offers the possibility to add outliers (level shift) in the
#' Benchmark-to-Indicator (BI) ratio and import infra-annual BI ratio manually
#' for some periods. The function also includes an automatic procedure to
#' forecast the annual BI ratio and takes this forecast into account in the
#' estimation of the out-of-sample periods. Instead of the automatic procedure,
#' forecasts of the annual BI ratio can also be defined by the user.
#'
#' @param benchmarks set of annual benchmark series to disaggregate. It can be
#'   either a ts object or a data.frame. In the second case, the first column
#'   should be in a format that can be translated into a Date object. Only
#'   annual benchmarks are handled.
#' @param indicators set of monthly or quarterly indicators to use for the
#'   disaggregation of the annual benchmark series. It can be either a ts object
#'   or a data.frame. Indicators name must start with the name of the related
#'   benchmark series. However, the name may be extended using an underscore,
#'   for example when we'd like to use more than one indicator in a Chow-Lin
#'   model. Only quarterly or monthly indicators are handled.
#' @param model Temporal disaggregation model to consider for each series.
#'   Possible models are currently "mbdenton", "chow-lin", "fernandez" and
#'   "litterman". The model to use can be a character string (when the same
#'   model must be used for all series) or a list of structured definition of
#'   the models. The name of the series must be submitted first. This must be
#'   followed by an equal sign and the model's name to use for the series.
#' @param outliers Level shift(s) in the Benchmark-to-Indicator ratio for the
#'   model-based Denton. Must be specified as a list of structured definition of
#'   the outlier periods and their intensity for each series. The name of the
#'   series must be submitted first. This must be followed by an equal sign and
#'   the outliers description inside a vector (i.e., c(o1,o2,etc.)). The
#'   outliers description must start with the period formatted as YYYYMMDD and
#'   enclosed in quotation marks, then an equal sign and the intensity of the
#'   outlier, defined as the relative value of the 'innovation variances' (1=
#'   normal situation)
#' @param disagBIfixed Fix some disaggragated Benchmark-to-Indicator (BI) ratios
#'   for the model-based Denton. They must be specified as a list of structured
#'   definition of the periods of the disaggragated BI ratios and their values
#'   for each series. The name of the series must be submitted first. This must
#'   be followed by an equal sign and the disaggragated BI ratios inside a
#'   vector (i.e., c(bi1,bi2,etc.)). The disaggragated BI ratio must start with
#'   the period formatted as YYYYMMDD and enclosed in quotation marks, then an
#'   equal sign and the value of the BI ratio at that period. When fixed
#'   disaggregated BI ratio are provided, they must be provided for each
#'   quarter/month of the year.
#' @param forecastBI Forecast of the annual BI ratio to consider for the model-
#'   based Denton. If "none" is selected, no forecast is considered ('standard'
#'   model-based Denton). If "auto" is selected, an automatic forecast procedure
#'   of the annual BI ratio will be used. "userdefined+none" and
#'   "userdefined+auto" allows the user to specify the forecast of the annual BI
#'   ratio to consider for some series.
#' @param forecastBI.values Only used if userdefined forecast of the annual BI
#'   ratio are brought in. Manual forecast must be specified as a list of
#'   structured definition of the forecast values for each series. The name of
#'   the series must be submitted first. This must be followed by an equal sign
#'   and the forecasted values inside a vector of two values (i.e.,
#'   c(f_y+1,f_y+2)). The forecasted values must start with "Y+1" and "Y+2"
#'   respecively (enclosed in quotation marks), followed by an equal sign and
#'   the value of the forecast.
#' @param forecastBI.quantile quantile to consider when choosing between a
#'   random walk process and another model to forecast the annual BI ratio. A
#'   lower value of the quantile will increase the propensity to select another
#'   model and vice-versa.
#' @param freezeT1 a boolean object. By default, when freezeT1 = FALSE, the
#'   benchmarks series are automatically extended whenever the indicators series
#'   cover the year T+1. If freezeT1 = TRUE, the simple extrapolation is still
#'   used. That allows to avoid revision of the past of the series. Note that
#'   this is only relevant when the range of the indicators series covers
#'   exactly the year T+1. If it goes beyond (i.e. covering a part of year T+2),
#'   the argument is ignored and the benchmarks series are extended.
#' @param conversion type of consistency between the annual benchmarks and the
#'   infra-annual indicators
#' @param path_xlsx a character object containing the path of the XLSX file to
#'   generate with the main results. If NULL (default), no XLSX file is created.
#' @import data.table openxlsx forecast
#' @import rjd3toolkit rjd3bench rjd3sts rjd3tramoseats
#' @importFrom zoo as.Date.yearmon
#' @importFrom lubridate date_decimal
#' @return an object of class "nbb.multiTD.output"
#' @export
#' @examples
#' # retail data, 2 benchmarks series to disaggregate
#' Y<-cbind(rjd3toolkit::aggregate(rjd3toolkit::retail$RetailSalesTotal, 1),
#'          rjd3toolkit::aggregate(rjd3toolkit::retail$ClothingStores, 1))
#' colnames(Y)<-c("retail","clothing")
#' x<-cbind(rjd3toolkit::retail$FoodAndBeverageStore,
#'          rjd3toolkit::retail$AutomobileDealers,
#'          rjd3toolkit::retail$WomensClothingStores)
#' colnames(x)<-c("retail_food", "retail_cars", "clothing_womens")
#'
#' # Example 1: Chow-Lin (with 2 indicators) and standard Denton
#' rslt1<-multiTD(Y, x, model=list(retail="Chow-Lin", clothing="mbDenton"))
#'
#' # Example 2a: Model-based Denton with outliers (i.e., LS in the BI ratio)
#' #             on quarterly data
#' x2<-cbind(retail=rjd3toolkit::aggregate(x[,1],4), clothing=rjd3toolkit::aggregate(x[,3],4))
#' outliers = list(retail=c("20081001"=10, "20090101"=10),clothing=c("20081001"=10))
#' rslt2a<-multiTD(Y, x2, model="mbDenton", outliers=outliers)
#' # \dontrun{
#' # runShiny(rslt2a)
#' # }
#'
#' # Example 2b: Model-based Denton with outliers and fixed disaggregated BI ratio
#' # (adjustments based on the results of 2a)
#' outliers = list(retail=c("20081001"=10))
#' disagBIfixed = list(retail=c("20080101"=7.246695, "20080401"=7.246695,
#'                              "20080701"=7.246695, "20081001"=6.0))
#' rslt2b<-multiTD(Y, x2, model="mbDenton", outliers=outliers, disagBIfixed=disagBIfixed)
#' # \dontrun{
#' # runShiny(rslt2b)
#' # }
#'
#' # Example 3: Enhanced model-based Denton (i.e. use of forecast of the annual
#' # BI ratio in extrapolation). In this example, user-defined forecast for
#' # the series 'retail' and automatic forecast for the series 'clothing'
#' Y3<-window(Y, end=2009)
#' x3<-window(x2, end=c(2010,2))
#' forecastBI.values = list(retail=c("Y+1"=6.8, "Y+2"=7.0))
#' rslt3<-multiTD(Y3, x3, model="mbDenton", forecastBI = "userdefined+auto",
#'                forecastBI.values=forecastBI.values, forecastBI.quantile="q.99")
#'
multiTD <- function(benchmarks,
                    indicators,
                    model = "mbDenton",
                    outliers = NULL,
                    disagBIfixed = NULL,
                    forecastBI = c("none", "auto", "userdefined+none", "userdefined+auto"),
                    forecastBI.values = NULL,
                    forecastBI.quantile = c("q.95", "q.80", "q.90", "q.99", "q.999"),
                    freezeT1 = FALSE,
                    conversion = c("Sum", "Average"),
                    path_xlsx = NULL) {

    cl <- match.call()
    forecastBI <- match.arg(forecastBI)
    forecastBI.quantile <- match.arg(forecastBI.quantile)
    conversion <- match.arg(conversion)

    # I. Pre-processing

    ## Conversion of benchmarks and indicators to ts object
    benchmarks_ts<-convert_to_ts(benchmarks)
    indicators_ts<-convert_to_ts(indicators)
    freq<-frequency(indicators_ts)
    if(!freq %in% c(4,12))
        stop("only monthly or quarterly indicators are allowed")
    if(!frequency(benchmarks_ts) == 1)
        stop("only annual benchmarks are allowed")
    if(time(indicators_ts)[1]%%1!=0)
        stop("indicator series must start at the beginning of the year")
    if(!all(start(benchmarks_ts) == start(indicators_ts)))
        stop("benchmarks series and indicators do not start at the same time")

    ## Missing values
    indicators_ts <- replace_empty_col_by_cst(indicators_ts)
    if(any(is.na(indicators_ts)) | any(is.na(benchmarks_ts)))
        stop("missing observations are not handled")

    ## Critical value for forecasting annual BI
    nobs_bench<-min(if(is.matrix(benchmarks_ts)) nrow(benchmarks_ts) else length(benchmarks_ts),100)
    qx<-table_rw[series_length == nobs_bench, get(forecastBI.quantile)]

    # II. Processing

    out <- list()
    class(out) <- "nbb.multiTD.output"
    if(is.matrix(benchmarks_ts)){
        nc<-ncol(benchmarks_ts)
        nr<-ifelse(is.matrix(indicators_ts), nrow(indicators_ts), length(indicators_ts))
        nrb<-nrow(benchmarks_ts)
    }else{
        nc<-1
        nr<-ifelse(is.matrix(indicators_ts), nrow(indicators_ts), length(indicators_ts))
        nrb<-length(benchmarks_ts)
    }
    td_series<-td_series_stderr<-td_bi<-td_bi_stderr<-data.frame(matrix(nrow=nr,ncol=nc))
    colnames(td_series)<-colnames(td_series_stderr)<-colnames(td_bi)<-colnames(td_bi_stderr)<-colnames(benchmarks_ts)
    bi_annual<-data.frame(matrix(nrow=nrb,ncol=nc))
    bi_annual_f<-data.frame(matrix(nrow=2,ncol=nc))
    bi_annual_falt<-data.frame(matrix(nrow=2,ncol=nc))
    colnames(bi_annual)<-colnames(bi_annual_f)<-colnames(bi_annual_falt)<-colnames(benchmarks_ts)
    td_models<-data.frame(matrix(nrow=nc,ncol=1))
    colnames(td_models)<-"td_model"
    rownames(td_models)<-colnames(benchmarks_ts)
    fit<-data.frame(matrix(nrow=nc,ncol=4))
    rownames(fit)<-colnames(benchmarks_ts)
    colnames(fit)<-c("Ols %Y on %X tstat","Ols %Y on %X pval","Ols %BI on %X tstat","Ols %BI on %X pval")
    decomposition <- vector("list", nc)
    names(decomposition) <- colnames(benchmarks_ts)

    for(i in 1:nc){

        ## Data
        bnamei<-if(is.matrix(benchmarks_ts)) colnames(benchmarks_ts)[i] else "series"
        yi<-if(is.matrix(benchmarks_ts)) benchmarks_ts[,i,drop=FALSE] else benchmarks_ts
        xi<-match_indicators(bnamei, indicators_ts)
        xi1<-if(is.matrix(xi)) xi[,1] else xi
        xi1_is<-ts(xi1, frequency = freq, start = start(yi), end = c(end(yi)[1], freq))
        ne<-length(xi1)-length(xi1_is)

        ## Annual BI ratios
        xi_main<-if(is.matrix(xi)) xi[,1] else xi
        biYi <- calc_annual_bi_ratio(yi, xi_main, conversion)

        ## Select model
        if(is.list(model)){
            modi<-tolower(model[names(model)==bnamei])
            if(length(modi) == 0){
                modi<-"mbdenton"
                warning(paste0(bnamei, ": model not defined. mbDenton is used by default."), call. = FALSE)
            }
        }else{
            modi<-tolower(model)
        }
        if(!modi %in% c("mbdenton", "chow-lin", "fernandez", "litterman")){
            modi<-"mbdenton"
            warning(paste0(bnamei, ": model not allowed. mbDenton was used instead by default."), call. = FALSE)
        }

        if(modi == "mbdenton" & is.matrix(xi)){
            xi<-xi[,1]
            warning(paste0(bnamei, ": mbDenton do not handle multiple indicators. Only the first indicator is considered."), call. = FALSE)
        }

        ## Outliers
        if(is.null(outliers)){
            outli<-outli_int<-NULL
        }else{
            if(bnamei %in% names(outliers)){
                outli_int<-outliers[names(outliers)==bnamei][[1]]
                outli_char<-names(outli_int)
                outli<-sapply(outli_char, function(x) decimal_date2(as.numeric(substr(x,1,4)), as.numeric(substr(x,5,6)), freq))
            }else{
                outli<-outli_int<-NULL
            }
        }

        ## Fixed disagregated BI
        if(is.null(disagBIfixed)){
            dBIfixi<-NULL
        }else{
            if(bnamei %in% names(disagBIfixed)){
                dBIfixi_value<-disagBIfixed[names(disagBIfixed)==bnamei][[1]]
                dBIfixi_date_char<-names(dBIfixi_value)
                dBIfixi_date<-sapply(dBIfixi_date_char, function(x) decimal_date2(as.numeric(substr(x,1,4)), as.numeric(substr(x,5,6)), freq))
                dBIfixi<-cbind(period=dBIfixi_date, bi_ratio=dBIfixi_value)

                if(length(dBIfixi_value)%%freq != 0){
                    warning(paste0(bnamei, ": the fixed disaggregated BI ratio provided do not cover the entire year. They are ignored for the series."), call. = FALSE)
                    dBIfixi<-NULL
                }
            }else{
                dBIfixi<-NULL
            }
        }

        ## Forecast annual BI
        if(forecastBI == "none"){
            fbiYi<-list(f=NULL,falt=NULL)
        }else if(forecastBI == "auto"){
            fbiYi<- if(modi == "mbdenton") forecast_annual_bi(biYi,critical_value=qx) else list(f=NULL,falt=NULL)
        }else if(forecastBI == "userdefined+none"){
            f_user<-forecastBI.values[names(forecastBI.values)==bnamei]
            f<-if(!length(f_user)==0) f_user[[1]] else NULL
            fbiYi<-list(f=f, falt=NULL)
        }else if(forecastBI == "userdefined+auto"){
            f_user<-forecastBI.values[names(forecastBI.values)==bnamei]
            if(!length(f_user)==0){
                f<-f_user[[1]]
                falt<-forecast_annual_bi(biYi,critical_value=qx)[[1]]
            } else{
                if(modi == "mbdenton"){
                    f<-forecast_annual_bi(biYi,critical_value=qx)[[1]]
                    falt<-forecast_annual_bi(biYi,critical_value=qx)[[2]]
                }else{
                    f<-falt<-NULL
                }
            }
            fbiYi<-list(f=f,falt=falt)
        }

        ## Model-based Denton
        if(modi == "mbdenton"){

            ### Standard model-based Denton
            if(is.null(fbiYi[[1]])){
                rslti<-mbdenton(xi, yi, outliers=outli, outliers.intensity=outli_int, manual_disagBI=dBIfixi, conversion)

                bi<-rslti[["beta"]]
                ebi<-rslti[["betaSD"]]
                disag<-rslti[["disag"]]
                edisag<-xi*ebi
                parameters<-cbind(beta_t=bi, stderr=ebi)
                decomp<-list(regeffect = disag, smoothingpart = ts(rep(0,length(disag)), start=start(disag), frequency = frequency(disag)))
                ll<-rslti[["ll"]]
                enhanced<-FALSE

                if(ne >= freq){
                    f_y1<-calc_implicit_forecast_annual_BI_ratio(disag, xi, start=end(yi)[1]+1, conversion=conversion)
                    fbiYi<-list(f=c("Y+1"=f_y1[1], "Y+2"=f_y1[2]), falt=NULL)
                }
            }

            ### 'Enhanced' model-based Denton
            else {
                #### extension of benchmark series when indicator covers the full year T+1
                if((ne>freq & ne<freq*2) | (ne==freq & !freezeT1)){
                    yi_enhanced<-extend_benchmark_series(yi, xi, fbiYi[[1]][1], conversion)
                }else if(ne>=freq*2){
                    stop("Out-of-sample period with enhanced model-based Denton must be < 2 years.")
                }else{
                    yi_enhanced<-yi
                }

                #### run model
                rslti<-mbdenton(xi, yi_enhanced, outliers=outli, outliers.intensity=outli_int, manual_disagBI=dBIfixi, conversion)

                #### extrapolation
                disagbi_base<-window(rslti[["beta"]], end=c(end(yi_enhanced)[1],freq))
                fbiYi_touse<-if(length(yi_enhanced)>length(yi)) as.numeric(fbiYi[[1]][2]) else as.numeric(fbiYi[[1]][1])
                disagbi<-extrapolate_infra_annual_BI_ratio(disagbi_base, fbiYi_touse, freq, yi_enhanced, xi, conversion)

                bi<-window(disagbi,frequency=freq,end=end(xi))
                ebi<-ts(c(window(rslti[["betaSD"]],frequency=freq,end=c(end(yi_enhanced)[1]-1,freq)), rep(NA,(ne+freq))),
                        frequency=freq,start=start(xi), end=end(xi))
                disag<-xi*bi
                edisag<-xi*ebi
                parameters<-cbind(beta_t=bi, stderr=ebi)
                decomp<-list(regeffect = disag, smoothingpart = ts(rep(0,length(disag)), start=start(disag), frequency = frequency(disag)))
                ll<-rslti[["ll"]]
                enhanced<-TRUE
            }

            ## Chow-Lin and variants
        }else if(modi %in% c("chow-lin", "fernandez", "litterman")){

            if(!is.null(outli)){
                warning(paste0(bnamei, ": outliers are only handled with mbDenton. The outlier(s) provided by the user were ignored for this series."), call. = FALSE)
            }
            if(!is.null(dBIfixi)){
                warning(paste0(bnamei, ": Fixed values for the disaggregated BI ratios are only possible with mbDenton. The value(s) provided by the user were ignored for this series."), call. = FALSE)
            }
            if(!is.null(fbiYi[[1]])){
                fbiYi<-list(f=NULL,falt=NULL)
                warning(paste0(bnamei, ": Forecast of annual BI ratio are only handled with mbDenton. The forecast values were ignored for this series."), call. = FALSE)
            }

            modi_short <- ifelse(modi == "chow-lin", "Ar1", ifelse(modi == "fernandez", "Rw", "RwAr1"))
            if(length(unique(xi) > 1)){
                rslti<-temporaldisaggregation(yi, indicators = as.list(xi), model = modi_short, conversion = conversion)
            }else{
                rslti<-temporaldisaggregation(yi, model = modi_short, conversion = conversion) # case without indicator
            }

            disag<-rslti$estimation$disagg
            edisag<-rslti$estimation$edisagg
            bi<-disag/xi1 # BI ratio of the first indicator
            ebi<-edisag/xi1
            parameters<-list(rho = rslti$estimation$parameter, coef = rslti$regression$model, cov = rslti$regression$cov)
            decomp<-list(regeffect = rslti$estimation$regeffect, smoothingpart = disag - rslti$estimation$regeffect)
            ll<-rslti$likelihood$ll

            if(ne >= freq){
                f_y1<-calc_implicit_forecast_annual_BI_ratio(disag, xi1, start=end(yi)[1]+1, conversion=conversion)
                fbiYi<-list(f=c("Y+1"=f_y1[1], "Y+2"=f_y1[2]), falt=NULL)
            }
        }

        # III. Tests

        ## Relevancy of outliers
        if(modi == "mbdenton" & !is.null(outli)){
            yi_used<-if(enhanced) yi_enhanced else yi
            rsltiNO<-mbdenton(xi, yi_used, outliers=NULL, manual_disagBI=dBIfixi, conversion=conversion)
            lr_test(ll1=ll, ll2=rsltiNO$ll, bnamei)
        }

        ## Check rho estimate with Chow-Lin
        if(modi %in% c("chow-lin", "litterman")){
            rho_test(rho=rslti$estimation$parameter, bnamei)
        }

        ## Fit in growth rate between the benchmarks and the (annualized) indicators and between the annual bi ratios and the (annualized) indicators
        ols_yi <- calc_fit_growth(yi, xi1)
        ols_biYi <- calc_fit_growth(biYi, xi1)

        # IV. Fill output
        out[[i]]<-list(td_model=modi, disag=disag, edisag=edisag, bi=bi, ebi = ebi,
                       parameters=parameters, decomposition=decomp, ll=ll)
        names(out)[i]<-bnamei
        td_series[,i]<-disag
        td_series_stderr[,i]<-edisag
        td_bi[,i]<-bi
        td_bi_stderr[,i]<-ebi
        bi_annual[,i]<-biYi
        if(!is.null(fbiYi$f)) bi_annual_f[,i]<-fbiYi$f
        if(!is.null(fbiYi$falt)) bi_annual_falt[,i]<-fbiYi$falt
        td_models[i,1]<-modi
        fit[i,1]<-ols_yi$t_stat
        fit[i,2]<-ols_yi$p_value
        fit[i,3]<-ols_biYi$t_stat
        fit[i,4]<-ols_biYi$p_value
        decomposition[[i]]<-decomp
    }

    out$call <- cl
    out$benchmarks <- benchmarks_ts
    out$indicators <- indicators_ts
    out$td_models <- td_models
    out$td_series <- ts(td_series, frequency = freq, start=start(indicators_ts))
    out$td_series_stderr <- ts(td_series_stderr, frequency = freq, start=start(indicators_ts))
    out$td_bi <- ts(td_bi, frequency = freq, start=start(indicators_ts))
    out$td_bi_stderr <- ts(td_bi_stderr, frequency = freq, start=start(indicators_ts))
    out$bi_annual <- ts(bi_annual, frequency = 1, start=start(benchmarks_ts))
    out$bi_annual_f <- ts(bi_annual_f, frequency = 1, start=end(benchmarks_ts)[1]+1)
    out$bi_annual_falt <- ts(bi_annual_falt, frequency = 1, start=end(benchmarks_ts)[1]+1)
    out$decomposition <- decomposition
    out$fit <- fit
    out$conversion <- conversion

    if(!is.null(path_xlsx)){
        wb <- createWorkbook()
        addWorksheet(wb, "td_series")
        addWorksheet(wb, "td_bi")
        addWorksheet(wb, "bi_annual")
        addWorksheet(wb, "bi_annual_f")
        addWorksheet(wb, "bi_annual_falt")
        addWorksheet(wb, "td_models")
        addWorksheet(wb, "check_fit")

        writeData(wb, "td_series", data.frame(date=as.Date.yearmon(time(indicators_ts)), out$td_series), startRow = 1, startCol = 1)
        writeData(wb, "td_bi", data.frame(date=as.Date.yearmon(time(indicators_ts)), out$td_bi), startRow = 1, startCol = 1)
        writeData(wb, "bi_annual", data.frame(date=as.Date.yearmon(time(benchmarks_ts)), out$bi_annual), startRow = 1, startCol = 1)
        writeData(wb, "bi_annual_f", data.frame(date=as.Date.yearmon(time(out$bi_annual_f)), out$bi_annual_f), startRow = 1, startCol = 1)
        writeData(wb, "bi_annual_falt", data.frame(date=as.Date.yearmon(time(out$bi_annual_falt)), out$bi_annual_falt), startRow = 1, startCol = 1)
        writeData(wb, "td_models", td_models, startRow = 1, startCol = 1)
        writeData(wb, "check_fit", fit, startRow = 1, startCol = 1)

        saveWorkbook(wb, file = path_xlsx, overwrite = TRUE)
    }

    return(out)
}


#'
#' Multiprocessing temporal disaggregation of time series with input coming
#' from a structured XLSX file
#'
#' See ?multiTD for more description. See vignette or the template for
#' explanation about the required structure of the XLSX file.
#'
#' @param path_data a string containing the path of the input XLSX file.
#' @param forecastBI Forecast of the annual BI ratio to consider for the model-
#'   based Denton. If "none" is selected, no forecast is considered ('standard'
#'   model-based Denton). If "auto" is selected, an automatic forecast procedure
#'   of the annual BI ratio will be used. "userdefined+none" and
#'   "userdefined+auto" allows the user to specify the forecast of the annual BI
#'   ratio to consider for some series.
#' @param forecastBI.quantile quantile to consider when choosing between a
#'   random walk process and another model to forecast the annual BI ratio. A
#'   lower value of the quantile will increase the propensity to select another
#'   model and vice-versa.
#' @param freezeT1 a boolean object. By default, when freezeT1 = FALSE, the
#'   benchmarks series are automatically extended whenever the indicators series
#'   cover the year T+1. If freezeT1 = TRUE, the simple extrapolation is still
#'   used. That allows to avoid revision of the past of the series. Note that
#'   this is only relevant when the range of the indicators series covers
#'   exactly the year T+1. If it goes beyond (i.e. covering a part of year T+2),
#'   the argument is ignored and the benchmarks series are extended.
#' @param conversion type of consistency between the annual benchmarks and the
#'   infra-annual indicators
#' @param path_output a string containing the path of the output (which is also
#'   an XLSX file) containing the results. If the path is NULL, no file is
#'   created.
#'
#' @import readxl
#' @return an object of class "nbb.multiTD.output"
#' @export
#' @examples
#' \dontrun{
#' file.name<-"myinput.xlsx"
#' res<-multiTD_fromXLSX(path_data = "input_to_R.xlsx",
#'                       forecastBI = "userdefined+auto",
#'                       forecastBI.quantile = "q.95",
#'                       freezeT1 = FALSE,
#'                       conversion = "Sum",
#'                       path_output = "output_R.xlsx")
#' }
#'
multiTD_fromXLSX <- function(path_data,
                             forecastBI = c("none", "auto", "userdefined+none", "userdefined+auto"),
                             forecastBI.quantile = c("q.95", "q.80", "q.90", "q.99", "q.999"),
                             freezeT1 = FALSE,
                             conversion = c("Sum", "Average"),
                             path_output = NULL){

    forecastBI <- match.arg(forecastBI)
    forecastBI.quantile <- match.arg(forecastBI.quantile)
    conversion <- match.arg(conversion)

    # I. Import input and formatting

    benchmarks<-as.data.frame(read_excel(path_data, sheet = "benchmarks"))
    indicators<-as.data.frame(read_excel(path_data, sheet = "indicators"))

    models<-as.data.frame(read_excel(path_data, sheet = "models"))
    if(nrow(models) != 0){
        models_list_form<-split(models$model, seq(nrow(models)))
        names(models_list_form)<-models$series_name
    }else{
        warning("No model defined in the input file. mbDenton was used for each series by default.", call. = FALSE)
        models_list_form<-"mbDenton"
    }

    outliers<-as.data.frame(read_excel(path_data, sheet = "outliers"))
    if(nrow(outliers) != 0){
        outliers_list<-split(outliers[,c("period","intensity")], outliers$series_name)
        outliers_list_form<-lapply(outliers_list, function(x)  df_to_vector(x))
    }else{
        outliers_list_form<-NULL
    }

    disag_BI_fixed<-as.data.frame(read_excel(path_data, sheet = "disag_BI_fixed"))
    if(nrow(disag_BI_fixed) != 0){
        disag_BI_fixed_list<-split(disag_BI_fixed[,c("period","value")], disag_BI_fixed$series_name)
        disag_BI_fixed_list_form<-lapply(disag_BI_fixed_list, function(x)  df_to_vector(x))
    }else{
        disag_BI_fixed_list_form<-NULL
    }

    forecast_annual_BI<-as.data.frame(read_excel(path_data, sheet = "forecast_annual_BI"))
    if(nrow(forecast_annual_BI) != 0){
        colnames(forecast_annual_BI)<-c("series_name","Y+1","Y+2")
        forecast_annual_BI_list<-split(forecast_annual_BI[,2:3], seq(nrow(forecast_annual_BI)))
        forecast_annual_BI_list_form<-lapply(forecast_annual_BI_list, function(x) unlist(x[1,]))
        names(forecast_annual_BI_list_form)<- forecast_annual_BI$series_name
    }else{
        forecast_annual_BI_list_form<-NULL
    }


    # II. Run main function

    rslt<-multiTD(benchmarks,
                  indicators,
                  model=models_list_form,
                  outliers=outliers_list_form,
                  disagBIfixed=disag_BI_fixed_list_form,
                  forecastBI,
                  forecastBI.values=forecast_annual_BI_list_form,
                  forecastBI.quantile,
                  freezeT1,
                  conversion=conversion,
                  path_xlsx = path_output)

    return(rslt)
}


# Convert df to ts object
convert_to_ts<-function(x){
    input_name<-deparse(substitute(x))
    if(is.ts(x)){
        x_ts <- x
    } else if(is.data.frame(x)){
        if(!ncol(x) > 1) stop(paste0(input_name, ": missing input!"))
        first_date <- x[[1,1]]
        if(is_convertible_to_date(first_date)){
            first_yr <- as.numeric(substr(first_date,1,4))
            colnames(x)<-c("DATE",colnames(x)[-1])
            all_dates_yr <- as.numeric(substr(x$DATE,1,4))
            freq <- length(all_dates_yr[all_dates_yr == first_yr+1])
            first_infra <- freq - length(all_dates_yr[all_dates_yr == first_yr]) + 1
            x_ts <- ts(x[,-1], start = c(first_yr, first_infra), frequency = freq)
        } else stop(paste0(input_name, ": First column of the input data frame cannot be converted to a date object."))
    } else stop(paste0(input_name, ": Input series should be either a data frame or a ts object"))
    return(x_ts)
}

# Check if character argument can be converted to a 'Date' object
is_convertible_to_date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))

# Replace an empty column in a data frame or a ts object by a constant
replace_empty_col_by_cst <- function(t){
    f_rep <- function(x){
        if(all(is.na(x))){
            x[is.na(x)] <- 1 #could be any constant
        } else{
            x
        }
        return(as.numeric(x))
    }

    if(is.matrix(t)){
        t_clean <- ts(as.data.frame(sapply(t, f_rep)), frequency = frequency(t), start = start(t))
    }else{
        t_clean <- ts(as.vector(sapply(t, f_rep)), frequency = frequency(t), start = start(t))
    }

    return(t_clean)
}

# Find indicator(s) related to a benchmark
match_indicators<-function(bname, indicators_ts){
    if(is.matrix(indicators_ts)){
        iname<-colnames(indicators_ts)[colnames(indicators_ts)==bname]
        if(length(iname)>1){
            warning(paste0(benchmark_name, ": has multiple indicators with the same names. Only the first indicator is used."), call. = FALSE)
            xi<-indicators_ts[,iname]
        }else if(length(iname)==1){
            xi<-indicators_ts[,iname]
        }else{
            n<-nchar(bname)+1
            xi<-indicators_ts[,substr(colnames(indicators_ts),1,n)==paste0(bname,"_")]
            if(is.matrix(xi)){
                if(ncol(xi)==0){
                    warning(paste0(bname, ": no indicator found for this series. A smoothing was performed."), call. = FALSE)
                    xi<-ts(rep(1,length(time(indicators_ts))), start=start(indicators_ts), frequency = frequency(indicators_ts))
                }
            }
        }
    }else{
        xi<-indicators_ts
    }
    return(xi)
}

# Format date in decimal
decimal_date2 <- function(yr, mth, freq){
    if(freq == 12){
        as.numeric(time(ts(start = c(yr, mth), frequency = freq)))
    }else if (freq == 4){
        qtr<-ifelse(mth<=3,1,
                    ifelse(mth<=6,2,
                           ifelse(mth<=9,3,4)))
        as.numeric(time(ts(start = c(yr, qtr), frequency = freq)))
    }else{
        NULL
        warning("frequency not handled")
    }
}

#' Calculate annual BI ratio
#'
#' @param s ts or mts object annual benchmark(s)
#' @param i ts or mts object infra-annual indicator(s)
#' @param conversion a character object indicating the type of conversion
#' @return ts or mts object annual BI ratio(s)
calc_annual_bi_ratio <- function(s, i, conversion = c("Sum", "Average")){

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

# Calculate implicit forecasts of annual BI ratio.
calc_implicit_forecast_annual_BI_ratio <- function(td_series, indicator, start, conversion = c("Sum", "Average")) {

    freq <- frequency(td_series)

    # keep extrapolation period
    q <- window(td_series, start = start)
    i <- window(indicator, start = start)

    # annual aggregation
    if(conversion == "Sum"){
        qY <- aggregate.ts(q, nfrequency = 1, FUN = sum)
        iY <- aggregate.ts(i, nfrequency = 1, FUN = sum)
    } else if (conversion == "Average"){
        qY <- aggregate.ts(q, nfrequency = 1, FUN = mean)
        iY <- aggregate.ts(i, nfrequency = 1, FUN = mean)
    }

    bi_f<-if(iY != 0) as.numeric(qY/iY) else NA

    return(bi_f)
}


# Likelihood ratio test
lr_test<-function(ll1,ll2,series_name){
    test_value <- 2*abs(ll1 - ll2)
    critical_value <- qchisq(p = 0.05, df = 1, lower.tail=FALSE)
    if(test_value < critical_value){
        warning(paste0(series_name, ": No evidence of superiority of the model with outlier(s) compared with the model without outlier."), call. = FALSE)
    }
}

# Test value of rho with Chow-Lin and Litterman
rho_test<-function(rho, series_name){
    if(rho == 0){
        warning(paste0(series_name, ": The MLE of rho is 0. Please be aware that this is equivalent to a pure OLS model with potentiel step effects."), call. = FALSE)
    } else if (rho < 0){
        warning(paste0(series_name, ": !!! The MLE of rho is negative 0. This is very likely to give unexpected results. You should consider another model for this series such as Fernandez. !!!"), call. = FALSE)
    }
}

# Compute growth and perform OLS
calc_fit_growth <- function(y, x){

    if(length(unique(x)) == 1) {
        t_stat <- p_value <- NA
    }else{
        ## annual growth rates
        xY <- window(aggregate(x, nfreq = 1), start = start(y), end = end(y))
        yg <- y / lag(y,-1) - 1
        xYg <- xY / lag(xY,-1) - 1

        ## OLS
        fit <- tslm(yg ~ xYg)
        res <- summary(fit)

        ## collect tstat
        t_stat <- round(res$coefficients[2,3], 3)
        p_value <- round(res$coefficients[2,4], 5)
    }

    return(list(t_stat=t_stat,p_value=p_value))
}

# Convert 2 columns data.frame to named vector
df_to_vector<-function(x){
    x_form <- x[,2]
    names(x_form) <- x[,1]
    return(x_form)
}
