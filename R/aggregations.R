#' @title hourlyAgg
#'
#' @description Aggregates sub hourly time series into an hourly resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the HydroEnR package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' hourlyAgg(Buildwas)
hourlyAgg <- function(x, method = 'mean', ...) {
  UseMethod('hourlyAgg', x)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.flowLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x$GaugeData[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x$GaugeData[, .(Hourly_Median = median(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x$GaugeData[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x$GaugeData[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x$GaugeData[, .(Hourly_Sum = sum(Volume, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.stageLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x$GaugeData[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x$GaugeData[, .(Hourly_Median = median(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x$GaugeData[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x$GaugeData[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.rainLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x$GaugeData[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x$GaugeData[, .(Hourly_Median = median(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x$GaugeData[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x$GaugeData[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x$GaugeData[, .(Hourly_Sum = sum(Volume, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.rainAll <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x[, lapply(.SD, mean, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x[, lapply(.SD, median, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x[, lapply(.SD, min, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x[, lapply(.SD, max, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x[, lapply(.SD, sum, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  class(Hourly)[3] <- 'rainAllHourly'
  return(Hourly)
}

#' @title dailyAgg
#'
#' @description Aggregates sub daily time series into a daily resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the HydroEnR package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' dailyAgg(Buildwas)
dailyAgg <- function(x, method = 'mean', ...) {
  UseMethod('dailyAgg', x)
}

#' @rdname dailyAgg
#' @export
dailyAgg.flowLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Daily <- x$GaugeData[, .(Daily_Mean = mean(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'median') {
    Daily <- x$GaugeData[, .(Daily_Median = median(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'min') {
    Daily <- x$GaugeData[, .(Daily_Min = min(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'max') {
    Daily <- x$GaugeData[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'sum') {
    Daily <- x$GaugeData[, .(Daily_Sum = sum(Volume, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.rainLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Daily <- x$GaugeData[, .(Daily_Mean = mean(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'median') {
    Daily <- x$GaugeData[, .(Daily_Median = median(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'min') {
    Daily <- x$GaugeData[, .(Daily_Min = min(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'max') {
    Daily <- x$GaugeData[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'sum') {
    Daily <- x$GaugeData[, .(Daily_Sum = sum(Volume, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.stageLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Daily <- x$GaugeData[, .(Daily_Mean = mean(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'median') {
    Daily <- x$GaugeData[, .(Daily_Median = median(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'min') {
    Daily <- x$GaugeData[, .(Daily_Min = min(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'max') {
    Daily <- x$GaugeData[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.rainAll <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Daily <- x[, lapply(.SD, mean, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if(method  == 'median') {
    Daily <- x[, lapply(.SD, median, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if(method  == 'min') {
    Daily <- x[, lapply(.SD, min, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if(method  == 'max') {
    Daily <- x[, lapply(.SD, max, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if(method  == 'sum') {
    Daily <- x[, lapply(.SD, sum, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  class(Daily)[3] <- 'rainAllDaily'
  return(Daily)
}

#' @title monthlyAgg
#'
#' @description Aggregates sub monthly time series into a monthly resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the HydroEnR package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' monthlyAgg(Buildwas)
monthlyAgg <- function(x, method = 'mean', ...) {
  UseMethod('monthlyAgg', x)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.flowLoad <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x$GaugeData[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x$GaugeData[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x$GaugeData[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x$GaugeData[, .(Monthly_Sum = sum(Volume, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  return(Monthly)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.rainLoad <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x$GaugeData[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x$GaugeData[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x$GaugeData[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x$GaugeData[, .(Monthly_Sum = sum(Volume, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  return(Monthly)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.stageLoad <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x$GaugeData[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x$GaugeData[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x$GaugeData[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Monthly)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.rainAll <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Monthly <- x[, lapply(.SD, mean, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x[, lapply(.SD, median, na.rm = TRUE), .(Year_Month = paste(year(Date), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x[, lapply(.SD, min, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x[, lapply(.SD, max, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x[, lapply(.SD, sum, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  class(Monthly)[3] <- 'rainAllMonthly'
  return(Monthly)
}


#' @title annualAgg
#'
#' @description Aggregates sub annual time series into an annual resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the HydroEnR package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' annualAgg(Buildwas)
annualAgg <- function(x, method = 'mean', ...) {
  UseMethod('annualAgg', x)
}

#' @rdname annualAgg
#' @export
annualAgg.flowLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Annual <- x$GaugeData[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'median') {
    Annual <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'min') {
    Annual <- x$GaugeData[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'max') {
    Annual <- x$GaugeData[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'sum') {
    Annual <- x$GaugeData[, .(Annual_Sum = sum(Volume, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.rainLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Annual <- x$GaugeData[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'median') {
    Annual <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'min') {
    Annual <- x$GaugeData[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'max') {
    Annual <- x$GaugeData[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'sum') {
    Annual <- x$GaugeData[, .(Annual_Sum = sum(Volume, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.stageLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Annual <- x$GaugeData[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'median') {
    Annual <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'min') {
    Annual <- x$GaugeData[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'max') {
    Annual <- x$GaugeData[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.rainAll <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Annual <- x[, lapply(.SD, mean, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'median') {
    Annual <- x[, lapply(.SD, median, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'min') {
    Annual <- x[, lapply(.SD, min, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'max') {
    Annual <- x[, lapply(.SD, max, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if(method  == 'sum') {
    Annual <- x[, lapply(.SD, sum, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  class(Annual)[3] <- 'rainAllYear'
  return(Annual)
}

#' @title hydroYearAgg
#'
#' @description Aggregates sub annual time series into a hydrological year resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the HydroEnR package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' hydroYearAgg(Buildwas)
hydroYearAgg <- function(x, method = 'mean', ...) {
  UseMethod('hydroYearAgg', x)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.flowLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'median') {
    Hydro_year <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'min') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'max') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'sum') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Sum = sum(Volume, na.rm = TRUE)), HydrologicalYear]
  }
  return(Hydro_year)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.rainLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'median') {
    Hydro_year <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'min') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'max') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'sum') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Sum = sum(Volume, na.rm = TRUE)), HydrologicalYear]
  }
  return(Hydro_year)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.stageLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'median') {
    Hydro_year <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'min') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'max') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Hydro_year)
}



#' @title rollingAggs
#'
#' @description Provides a rolling aggregation of a user defined periods.
#'
#' @param dt flowLoad, rainLoad, or stageLoad data
#' @param rolling_aggregations User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change to 1 etc.
#' @param method 'min', 'max', 'mean', 'median', and 'sum' options available
#'
#' @return
#' @export
#'
#' @import data.table
#' @import RcppRoll
#'
#' @examples
#' rollingAggs(Buildwas)
rollingAggs <- function(dt, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean') {
  UseMethod('rollingAggs', dt)
}

#' @rdname rollingAggs
#' @export
rollingAggs.flowLoad <- function(dt, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
  roller <- get(paste0("roll_", method))
  agg <- length(rolling_aggregations)
  if(method == 'sum'){
    Rolling_Aggregations <- data.table(DateTime = dt$GaugeData$DateTime, HydroYear = dt$GaugeData$HydrologicalYear, HydroDay = dt$GaugeData$day_of_hy, Raw = dt$GaugeData$Volume)
  } else {
    Rolling_Aggregations <- data.table(DateTime = dt$GaugeData$DateTime, HydroYear = dt$GaugeData$HydrologicalYear, HydroDay = dt$GaugeData$day_of_hy, Raw = dt$GaugeData$Value)
  }
  for(i in seq_along(rolling_aggregations)){
    window <- rolling_aggregations[i]/interval
    if(rolling_aggregations[i] %% interval > 0){
      cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
      Rolling_Aggregations[, paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "") := rep(NA, length(Rolling_Aggregations$DateTime))]
      next
    } else {
      window <- rolling_aggregations[i]/interval
    }

    cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
    Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
  }
  return(Rolling_Aggregations)
}

#' @rdname rollingAggs
#' @export
rollingAggs.rainLoad <- function(dt, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
  roller <- get(paste0("roll_", method))
  agg <- length(rolling_aggregations)
  if(method == 'sum'){
    Rolling_Aggregations <- data.table(DateTime = dt$GaugeData$DateTime, HydroYear = dt$GaugeData$HydrologicalYear, HydroDay = dt$GaugeData$day_of_hy, Raw = dt$GaugeData$Volume)
  } else {
    Rolling_Aggregations <- data.table(DateTime = dt$GaugeData$DateTime, HydroYear = dt$GaugeData$HydrologicalYear, HydroDay = dt$GaugeData$day_of_hy, Raw = dt$GaugeData$Value)
  }
  for(i in seq_along(rolling_aggregations)){
    window <- rolling_aggregations[i]/interval
    if(rolling_aggregations[i] %% interval > 0){
      cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
      Rolling_Aggregations[, paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "") := rep(NA, length(Rolling_Aggregations$DateTime))]
      next
    } else {
      window <- rolling_aggregations[i]/interval
    }

    cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
    Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
  }
  return(Rolling_Aggregations)
}

#' @rdname rollingAggs
#' @export
rollingAggs.stageLoad <- function(dt, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
  roller <- get(paste0("roll_", method))
  agg <- length(rolling_aggregations)
  if(method == 'sum'){
    stop('Stage data are not suitable for sumation')
  } else {
    Rolling_Aggregations <- data.table(DateTime = dt$GaugeData$DateTime, HydroYear = dt$GaugeData$HydrologicalYear, HydroDay = dt$GaugeData$day_of_hy, Raw = dt$GaugeData$Value)
  }
  for(i in seq_along(rolling_aggregations)){
    window <- rolling_aggregations[i]/interval
    if(rolling_aggregations[i] %% interval > 0){
      cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
      Rolling_Aggregations[, paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "") := rep(NA, length(Rolling_Aggregations$DateTime))]
      next
    } else {
      window <- rolling_aggregations[i]/interval
    }

    cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
    Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
  }
  return(Rolling_Aggregations)
}



#' @title Hydro Aggregate
#'
#' @details Wrapper function for the various aggregation methods employed in
#' HydroEnR
#'
#' @param dt flowLoad, rainLoad, or stageLoad data
#' @param rolling_aggregations User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change to 1 etc.
#' @param method 'min', 'max', 'mean', 'median', and 'sum' options available
#'
#' @return
#' @export
#'
#' @examples
#' #hydroAggregate(buildwas)
hydroAggregate <- function(dt, interval = 0.25, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), method = 'mean') {
  if(missingArg(dt)){
    stop("Data missing. Please supply data to run this function")
  }
  if("Value" %in% colnames(dt$GaugeData) == FALSE){
    stop("Values (flow) field missing from data.table")
  }
  if("DateTime" %in% colnames(dt$GaugeData) == FALSE){
    stop("DateTime field missing from data.table")
  }
  data_list <- list()
  if(interval<1) {
    cat("====================== Calculating hourly aggregations =====================\n")
    Hourly <- hourlyAgg(dt, method = method)
  } else {
    Hourly <- NA
  }
  data_list[['Hourly']] <- Hourly

  cat("====================== Calculating daily aggregations ======================\n")
  Daily <- dailyAgg(dt, method = method)
  data_list[['Daily']] <- Daily

  cat("====================== Calculating monthly aggregations ====================\n")
  Monthly <- monthlyAgg(dt, method = method)
  data_list[['Monthly']] <- Monthly

  cat("====================== Calculating annual aggregations =====================\n")
  Annual <- annualAgg(dt, method = method)
  data_list[['Annual']] <- Annual

  cat("====================== Calculating Hydro Year aggregations =================\n")
  Hydro_year <- hydroYearAgg(dt, method = method)
  data_list[['Hydro_year']] <- Hydro_year

  if(length(rolling_aggregations) > 0){
    Rolling_Aggregations <- rollingAggs(dt, interval = interval, rolling_aggregations = rolling_aggregations, method = method)
  }
  data_list[['Rolling_Aggregations']] <- Rolling_Aggregations
  class(data_list) <- append(class(data_list), c(paste('HydroAggs', method, sep = ''), 'HydroAggs'))
  return(data_list)
}
