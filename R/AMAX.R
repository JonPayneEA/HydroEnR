# AMAX Functions

# Default AMX function, user specified flow and date required
GetAMAX.numeric <- function(x = flow, Date = date, ...){
  if(is(Date, 'Date') == FALSE){ # To account for numerous classes
    Date <- as.Date(Date)
  }
  hydro_year <- 'oct_us_gb'
  hydroData <- hydroYearDay(Date, hy_cal = hydro_year)
  dt <- data.table(Date, hydroData, x)
  AMAX <- dt[, .(Hydro_year_Max = max(x, na.rm = TRUE)), HydrologicalYear]
  class(AMAX) <- append(class(AMAX), 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  return(AMAX)
}

# Extract AMAX table from HydroAggsmax object
GetAMAX.HydroAggsmax <- function(x){
  AMAX <- x$Hydro_year
  class(AMAX) <- append(class(AMAX), 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  return(AMAX)
}

# Extract AMAX from data just loaded in via loadAllFlow()
GetAMAX.FlowLoad <- function(Flows, ...){
  AMAX <- Flows[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  class(AMAX) <- append(class(AMAX)[1:2], 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  return(AMAX)
}

# Convert AMAX data from rnrfa into the AMAX class
zataTable.zoo <- function(x, index.name = "Date") {
  #stopifnot(class(x) == 'zoo')
  xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
  setNames(data.table(attr(x, 'index'), x, row.names=NULL), c(index.name,xn))
}

zataTable <- function(x, ...) {
  UseMethod('zataTable', x)
}

GetAMAX.zoo <- function(Flows, ...){
  AMAX <- zataTable(Flows)
  if(is(AMAX$Date, 'Date') == FALSE){ # To account for numerous classes
    AMAX$Date <- as.Date(AMAX$Date)
  }
  AMAX$Date <- hydroYearDay(AMAX$Date, hy_cal = 'oct_us_gb')[1]
  class(AMAX) <- append(class(AMAX), 'HydroAMAX')
  colnames(AMAX) <- c('Year', 'AMAX')
  AMAX <- AMAX[, .(AMAX = max(AMAX, na.rm = TRUE)), Year]
  return(AMAX)
}

GetAMAX <- function(Flows, ...) {
  UseMethod('GetAMAX', Flows)
}

# GetAMAX(Flows = Buildwas$Value, Date = Buildwas$DateTime)
# GetAMAX(Buildwas_Analysis)
# GetAMAX(Buildwas)
# rnrfa::get_ts(id = 2001, type = 'amax-flow') %>% GetAMAX()
# GetAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
