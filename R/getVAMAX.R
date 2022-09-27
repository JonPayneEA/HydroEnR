#' @title getVAMAX
#'
#' @description Extract annual maximum volume data from various data sources.
#'
#' @param x A dataset containing flow volume data, generated from HydroEnR functions
#' @param rolling_aggregations User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change to 1 etc.
#' @param method Locked to 'sum'
#' @param ... Extra parameters if required
#'
#' @return
#' @export
#'
#' @examples
#' #getVAMAX(buildwas)
#' #getVAMAX(buildwas_analysis)
getVAMAX <- function(x, ...) {
  UseMethod('getVAMAX', x)
}

#' @rdname getVAMAX
#' @export
getVAMAX.HydroAggssum <- function(x){
  # Hourly max
  date <- vapply(strsplit(x$Hourly$Hourly,' '),
                 `[`, 1, FUN.VALUE = character(1))
  yearHydro <- HydroYearDay(as.Date(date))
  hourly <- data.table(Volume = x$Hourly$Hourly_Sum, yearHydro)
  VAMAX_Hourly <- hourly[, .(VAMAX_Hourly = max(Volume, na.rm = TRUE)), HydrologicalYear]

  # Daily
  yearHydro <- HydroYearDay(x$Daily$Daily)
  daily <- data.table(Volume = x$Daily$Daily_Sum, yearHydro)
  VAMAX_Daily <- daily[, .(VAMAX_Daily = max(Volume, na.rm = TRUE)), HydrologicalYear]

  # Monthly
  date <- paste(gsub(' ', '-', x$Monthly$Year_Month), '-01', sep = '')
  yearHydro <- HydroYearDay(as.Date(date))
  monthly <- data.table(Volume = x$Monthly$Monthly_Sum, yearHydro)
  VAMAX_Monthly <- monthly[, .(VAMAX_Monthly = max(Volume, na.rm = TRUE)), HydrologicalYear]

  # Calendar year
  VAMAX_Calendar <- x$Annual[, .(VAMAX_Calendar = max(Annual_Sum, na.rm = TRUE)), Calendar_Year]

  # Hydrological Year
  VAMAX_HydroYear <- x$Hydro_year[, .(VAMAX_HydroYear = max(Hydro_year_Sum, na.rm = TRUE)), HydrologicalYear]

  # Rolling Aggs
  dt_clean <- x$Rolling_Aggregations[, c(-1, -3, -4),]
  VAMAX_Rolls <- dt_clean[, lapply(.SD, max, na.rm = TRUE), HydroYear]
  colnames(dt_clean) <- gsub('Roll', 'VAMAX', colnames(dt_clean))

  # Compile
  dt <- data.table(VAMAX_Hourly, VAMAX_Daily[,-1], VAMAX_Monthly[,-1],
                   VAMAX_Calendar[,-1], VAMAX_HydroYear[,-1],VAMAX_Rolls[,-1])
  class(dt) <- append(class(dt), 'HydroVAMAX')
  return(dt)
}

#' @rdname getVAMAX
#' @export
getVAMAX.flowLoad <- function(x, interval = 0.25, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120)){
  if('Volume' %in% colnames(x$GaugeData) == FALSE){
    asVol(x)
  }
  hydroAgg <- hydroAggregate(x,
                       interval = interval,
                       rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120),
                       method = 'sum')
  dt <- getVAMAX(hydroAgg)
  return(dt)
}
