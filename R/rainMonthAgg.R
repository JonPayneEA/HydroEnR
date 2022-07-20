rainMonthAgg<- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
  }
  if(method  == 'median') {
    Monthly <- x[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
  }
  if(method  == 'min') {
    Monthly <- x[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
  }
  if(method  == 'max') {
    Monthly <- x[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
  }
  if(method  == 'sum') {
    Monthly <- x[, .(Monthly_Sum = sum(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
  }
  return(Monthly)
}

rainDailyAgg <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Daily <- x[, .(Daily_Mean = mean(Value, na.rm = TRUE)), Date]
  }
  if(method  == 'median') {
    Daily <- x[, .(Daily_Median = median(Value, na.rm = TRUE)), Date]
  }
  if(method  == 'min') {
    Daily <- x[, .(Daily_Min = min(Value, na.rm = TRUE)), Date]
  }
  if(method  == 'max') {
    Daily <- x[, .(Daily_Max = max(Value, na.rm = TRUE)), Date]
  }
  if(method  == 'sum') {
    Daily <- x[, .(Daily_Sum = sum(Volume, na.rm = TRUE)), Date]
  }
  return(Daily)
}
  dailyAgg <- function(x, method = 'mean', ...) {
  UseMethod('dailyAgg', x)
}
