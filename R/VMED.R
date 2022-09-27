#' @title Generate a VMED estimate
#'
#' @description  Calculate the VMED value of a series of flow data loaded into
#' the environment via HydroEnR.
#'
#' @param x A series of flow data loaded into the environment via HydroEnR
#' @param rolling_aggregations User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change to 1 etc.
#' @param method Locked to 'sum'
#' @param ... Extra parameters if required
#'
#' @return
#' @export
#'
#' @examples
#' #VMED(Buildwas)
#' #VMED(Buildwas_Analysis)
#' #VMED(getVAMAX(buildwas_analysis))
VMED <- function(x,...) {
  UseMethod('VMED', x)
}

#' @rdname VMED
#' @export
VMED.numeric <- function(x){
  x <- median(x, na.rm = TRUE)
  return(x)
}

#' @rdname VMED
#' @export
VMED.HydroVAMAX <- function(x){
  dt_clean <- x[,-1,]
  dt <- dt_clean[, lapply(.SD, median, na.rm = TRUE), ]
  return(dt)
}

#' @rdname VMED
#' @export
VMED.HydroAggssum <- function(x, interval = 0.25, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120)){
  dt_aggs <- getVAMAX(x)
  dt <- VMED(dt_aggs)
  return(dt)
}

#' @rdname VMED
#' @export
VMED.flowLoad <- function(x, interval = 0.25, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120)){
  if('Volume' %in% colnames(x$GaugeData) == FALSE){
    asVol(x)
  }
  hydroAgg <- hydroAggregate(x,
                             interval = interval,
                             rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120),
                             method = 'sum')
  dt_v <- getVAMAX(hydroAgg)
  dt <- VMED(dt_v)
  return(dt)
}
