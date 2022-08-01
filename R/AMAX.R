#' @title getAMAX
#'
#' @description Extract annual maximum peak data from various data sources.
#'
#' @param x A dataset containing flow data, generated from HydroEnR functions
#' @param ... Extra parameters if required
#'
#' @return
#' @export
#'
#' @examples
#' getAMAX(Flows = Buildwas$Value, Date = Buildwas$DateTime)
#' getAMAX(Buildwas_Analysis)
#' getAMAX(Buildwas)
#' rnrfa::get_ts(id = 2001, type = 'amax-flow') %>% getAMAX()
#' getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
getAMAX <- function(x, ...) {
  UseMethod('getAMAX', x)
}

#' @rdname getAMAX
#' @export
getAMAX.numeric <- function(x = flow, Date = date, ...){
  if(is(Date, 'Date') == FALSE){ # To account for numerous classes
    Date <- as.Date(Date)
  }
  hydro_year <- 'oct_us_gb'
  hydroData <- hydroYearDay(Date, hy_cal = hydro_year)
  dt <- data.table(Date, hydroData, x)
  AMAX <- dt[, .(Hydro_year_Max = max(x, na.rm = TRUE)), HydrologicalYear]
  class(AMAX) <- append(class(x), 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  return(AMAX)
}

#' @rdname getAMAX
#' @export
getAMAX.HydroAggsmax <- function(x){
  AMAX <- data.table(Year = x$Hydro_year$HydrologicalYear, AMAX = x$Hydro_year$Hydro_year_Max)
  class(AMAX) <- append(class(AMAX), 'HydroAMAX')
  #colnames(AMAX) <- c('Year', 'AMAX')
  return(AMAX)
}

# Extract AMAX from data just loaded in via loadAllFlow()
#' @rdname getAMAX
#' @export
getAMAX.flowLoad <- function(x, ...){
  AMAX <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  class(AMAX) <- append(class(AMAX)[1:2], 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  return(AMAX)
}

#' @rdname getAMAX
#' @export
getAMAX.zoo <- function(x, ...){
  AMAX <- zataTable(x)
  if(is(AMAX$Date, 'Date') == FALSE){ # To account for numerous classes
    AMAX$Date <- as.Date(AMAX$Date)
  }
  AMAX$Date <- hydroYearDay(AMAX$Date, hy_cal = 'oct_us_gb')[1]
  class(AMAX) <- append(class(AMAX), 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  AMAX <- AMAX[, .(AMAX = max(AMAX, na.rm = TRUE)), Year]
  return(AMAX)
}

