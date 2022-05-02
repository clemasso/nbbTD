
#' Creating a config object to be used as input in the functions 'multiTD',
#' or as part in the functions 'multiTDDenton', 'multiTDCLvar'.
#'
#' @param model a data.frame object with 2 columns: 'series_name' and
#'              'model' containing the TD model to use for each series.
#'              Currently, the following models are handled: 'edenton',
#'              'denton', 'chow-lin', 'fernandez', 'litterman'.
#'              The data.frame should contain at least one row. If you attempt
#'              to use the same model for all series, you can use the keyword
#'              '_ALL_'. If you do not define a model for each series,
#'              'edenton' is used by default for the unspecified series
#'              with a warning. If model = NULL, a data.frame is created
#'              with 'eDenton' defined for each series by default.
#' @param f_bi_manual a data.frame object with 3 columns: 'series_name',
#'                   'f_T1' and 'f_T2' containing manual forecast of annual
#'                    BI ratio for years T+1 and T+2. If the data.frame is
#'                    empty or if f_bi_manual = NULL, the results of the
#'                    automatic selection of forecasting model is used by
#'                    default for each series.
#'                    Only relevant for 'eDenton' model. f_bi_manual is ignored
#'                    when other models are used with a warning message.
#'
#' @param bi_q_outliers a data.frame object with 4 columns: 'series_name',
#'                   'year', quarter_month' and 'intensity' containing manual
#'                   outliers (and their intensity). If the data.frame is
#'                   empty or if bi_q_outliers = NULL, no outlier are considered.
#'                   The column 'quarter_month' must be filled with values
#'                   from 1 to 4 for quarterly data and with values from 1 to 12
#'                   for monthly data. The column 'intensity' allows the user
#'                   to set up the intensity of the outlier. The default value
#'                   is 10 and a value of 1 means no outlier. The scale
#'                   between 1 to 10 is not linear but exponential.
#'
#' @param bi_q_manual a data.frame object with 14 columns: 'series_name',
#'                    'year', 'q1_m1', ..., 'q4_m4', 'm5', ..., 'm12' containing
#'                    manual BI ratios (on an infra-annual basis). If the
#'                    data.frame is empty or if bi_q_manual = NULL, no manual
#'                    BI ratios are considered. The columns 'm5' to 'm12' must
#'                    remain empty in case of quarterly data.
#'
#' @return returns an object of class 'nbb.dsc.td.multiproc.config'
#' @export
#' @examples
#' model <- data.frame (series_name  = c("a","b","c"),
#'                      model = c("edenton","chow-lin","denton"))
#' f_bi_manual <- data.frame (series_name  = c("a","b","c"),
#'                            f_T1 = c(0.8,0.5,10),
#'                            f_T2 = c(0.82,0.5,9.9))
#' bi_q_outliers <- data.frame (series_name  = c("a","b","c"),
#'                              year = c(2020,2020,2016),
#'                              quarter_month = c(2,2,4),
#'                              intensity = c(10,10,10)) # keep intensity=10 by default
#' bi_q_manual <- data.frame(series_name  = c("a","b","c"),
#'                            year = c(2020,2020,2016),
#'                            q1_m1 = c(0.8,0.5,10),
#'                            q2_m2 = c(0.7,0.4,8),
#'                            q3_m3 = c(0.84,0.55,11),
#'                            q4_m4 = c(0.85,0.54,11), # all quarters/months of the year must be provided
#'                            m5 = c(NA,NA,NA), # fill with NA from here on for quarterly data
#'                            m6 = c(NA,NA,NA),
#'                            m7 = c(NA,NA,NA),
#'                            m8 = c(NA,NA,NA),
#'                            m9 = c(NA,NA,NA),
#'                            m10 = c(NA,NA,NA),
#'                            m11 = c(NA,NA,NA),
#'                            m12 = c(NA,NA,NA))
#'
setConfig_default <- function(model = NULL, f_bi_manual = NULL, bi_q_outliers = NULL, bi_q_manual = NULL){

  if(is.null(model)){
    model <- data.frame(series_name = "_ALL_", model = "EDenton")
  } else{
    model <- check_df_structure(model, c("series_name", "model"))
    if(nrow(model) == 0) stop(paste0("data frame '", model, "' is empty. If you attempt to use EDenton for each series, please create a one row data.frame with the keyword '_ALL_' for series_name and define the model as 'EDenton'."))
  }

  if(is.null(f_bi_manual)){
    f_bi_manual <- data.frame(series_name = character(), f_T1 = numeric(), f_T2 = numeric())
  } else{
    f_bi_manual <- check_df_structure(f_bi_manual, c("series_name", "f_T1", "f_T2"))
  }

  if(is.null(bi_q_outliers)){
    bi_q_outliers <- data.frame(series_name = character(), year = numeric(), quarter_month = numeric(), intensity = numeric())
  } else{
    bi_q_outliers <- check_df_structure(bi_q_outliers, c("series_name", "year", "quarter_month", "intensity"))
  }

  if(is.null(bi_q_manual)){
    bi_q_manual <- data.frame(series_name = character(), year = numeric(), q1_m1 = numeric(), q2_m2 = numeric(), q3_m3 = numeric(), q4_m4 = numeric(),
                                m5 = numeric(),	m6 = numeric(),	m7 = numeric(),	m8 = numeric(),	m9 = numeric(),	m10 = numeric(),	m11 = numeric(),	m12 = numeric())
  } else{
    bi_q_manual <- check_df_structure(bi_q_manual, c("series_name", "year", "q1_m1", "q2_m2", "q3_m3", "q4_m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12"))

    if(any(is.na(bi_q_manual[,1:6])) | (!all(is.na(bi_q_manual[,7:14])) & any(is.na(bi_q_manual[,7:14])))){
      stop("Missing values are found in 'bi_q_manual'. When infra-annual BI ratio are provided by the user, they must cover the entire year.")
    }
  }

  config <- list(model = model, f_bi_manual = f_bi_manual, bi_q_outliers = bi_q_outliers, bi_q_manual = bi_q_manual)
  class(config) <- "nbb.dsc.td.multiproc.config"

  return(config)
}


#' Creating a config object to be used as full input in the functions 'multiTD',
#' or as part in the functions 'multiTDDenton', 'multiTDCLvar'.
#'
#' @param file a character object with the path to the XLSX file containing
#'             the config settings to consider. The XLSX file must respect a
#'             specific structure. See help file of 'setConfig_default'
#'             for more information.
#' @return returns an object of class 'nbb.dsc.td.multiproc.config'
#' @export
#' @examples
#' \dontrun{
#' myconfig <- setConfig_FromXLSX(file = "data/xxx.xlsx")
#' }
#'
setConfig_FromXLSX <- function(file){

  model <- check_df_structure(read.xlsx(file, sheet = "model"), c("series_name", "model"))
  if(nrow(model) == 0) stop(paste0("data frame '", model, "' is empty. If you attempt to use EDenton for each series, please use the keyword '_ALL_' for series_name in the Excel file and define the model as EDenton."))
  f_bi_manual <- check_df_structure(read.xlsx(file, sheet = "f_bi_manual"), c("series_name", "f_T1", "f_T2"))
  bi_q_outliers <- check_df_structure(read.xlsx(file, sheet = "bi_q_outliers"), c("series_name", "year", "quarter_month", "intensity"))
  bi_q_manual <- check_df_structure(read.xlsx(file, sheet = "bi_q_manual"), c("series_name", "year", "q1_m1", "q2_m2", "q3_m3", "q4_m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12"))

  if(any(is.na(bi_q_manual[,1:6])) | (!all(is.na(bi_q_manual[,7:14])) & any(is.na(bi_q_manual[,7:14])))){
    stop("Missing values are found in 'bi_q_manual'. When infra-annual BI ratio are provided by the user, they must cover the entire year.")
  }

  config <- list(model = model, f_bi_manual = f_bi_manual, bi_q_outliers = bi_q_outliers, bi_q_manual = bi_q_manual)
  class(config) <- "nbb.dsc.td.multiproc.config"

  return(config)
}


# Validation of the df structure

check_df_structure <- function(df, varnames){

  if(!is.data.frame(df)) stop("df must be a data frame")

  df_name <- deparse(substitute(df))
  if(!all(varnames %in% colnames(df))) stop(paste0("one or more of the columns '", toString(varnames), "' were not found in the data frame '", df_name, "'"))
  if(sum(is.na(df[,1])) > 0) stop(paste0("the first column of the data frame '", df_name, "' contain NA values. The problem could come from empty lines added in the Excel file."))
  df_s <- subset(df, select=varnames)

  return(df_s)
}

