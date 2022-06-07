# hydroAggregate
# Aggregates data table created by loadAllFlow()
# Assumes a time step of 15mins or 0.25hrs - this can be edited
# Standard aggregations of hourly, daily, monthly, calendar year and hydrological year
# User defined rolling aggregations can be produced, all defined in hours
# Aggregation methods available mean, sum, min, max
# When max is selected AMAX plot is produced

hydroAggregate <- function(dt, interval = 0.25, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), method = 'mean') {
  if(missingArg(dt)){
    stop("Data missing. Please supply data to run this function")
  }
  if(class(Buildwas)[1] != "data.table"){
    stop("Please supply data as a data.table. Try loading data into R using loadAllFlow() to get correct format for analysis.")
  }
  if("Value" %in% colnames(dt) == FALSE){
    stop("Values (flow) field missing from data.table")
  }
  if("DateTime" %in% colnames(dt) == FALSE){
    stop("DateTime field missing from data.table")
  }
  if("Date" %in% colnames(dt) == FALSE){
    stop("Date field missing from data.table")
  }
  if("Hour" %in% colnames(dt) == FALSE){
    stop("Hour field missing from data.table")
  }
  
  agg <- length(rolling_aggregations)
  data_list <- list()
  if(method == 'mean'){
    
    if(interval<1){
      cat("====================== Calculating hourly aggregations =====================\n")
      Hourly <- dt[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(Date, Hour))]
    } else {
      Hourly <- NA
    }
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Mean = mean(Value, na.rm = TRUE)), Date]
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
    data_list[['Hydro_year']] <- Hydro_year
    
    if(is.null(rolling_aggregations)){
      stop
    } else {
      Rolling_Aggregations <- data.frame(DateTime = dt$DateTime, Raw = dt$Value)
      for(i in seq_along(rolling_aggregations)){
        window <- rolling_aggregations[i]/interval
        if(rolling_aggregations[i] %% interval > 0){
          cat("Using a rolling aggregation of ", i, "is not divisible by 0.25, skipping for next accumulation\n")
          Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "")] <- rep(NA, length(Rolling_Aggregations[1]))
          next
        } else {
          window <- rolling_aggregations[i]/interval
        }
        
        cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
        Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr", sep = "")] <- roll_mean(dt$Value, window, fill = NA)
      }
      data_list[['Rolling_aggs']] <- Rolling_Aggregations
    }
    
  } else if(method == 'sum'){
    
    if(interval<1){
      cat("====================== Calculating hourly aggregations =====================\n")
      Hourly <- dt[, .(Hourly_Sum = sum(Value, na.rm = TRUE)), .(Hourly = paste(Date, Hour))]
    } else {
      Hourly <- NA
    }
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Sum = sum(Value, na.rm = TRUE)), Date]
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Sum = sum(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Sum = sum(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Sum = sum(Value, na.rm = TRUE)), HydrologicalYear]
    data_list[['Hydro_year']] <- Hydro_year
    
    if(is.null(rolling_aggregations)){
      stop
    } else {
      Rolling_Aggregations <- data.frame(DateTime = dt$DateTime, Raw = dt$Value)
      for(i in seq_along(rolling_aggregations)){
        window <- rolling_aggregations[i]/interval
        if(rolling_aggregations[i] %% interval > 0){
          cat("Using a rolling aggregation of ", i, "is not divisible by 0.25, skipping for next accumulation\n")
          Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "")] <- rep(NA, length(Rolling_Aggregations[1]))
          next
        } else {
          window <- rolling_aggregations[i]/interval
        }
        
        cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
        Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr", sep = "")] <- roll_sum(dt$Value, window, fill = NA)
      }
      data_list[['Rolling_aggs']] <- Rolling_Aggregations
    }
    
  } else if(method == 'max'){
    
    if(interval<1){
      cat("====================== Calculating hourly aggregations =====================\n")
      Hourly <- dt[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(Date, Hour))]
    } else {
      Hourly <- NA
    }
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = Date)]
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
    data_list[['Hydro_year']] <- Hydro_year
    # QMED <- median(Hydro_year$Max)
    # data_list[['QMED']] <- noquote(paste("Estimated QMED: ", QMED))
    # p <- plot(data_list[[5]]$HydrologicalYear, data_list[[5]]$Max,
    #           xlab = "Hydrological Year",
    #           ylab = expression(Flow ~ m^3 ~ s^-1),
    #           main = "AMAX flow by hydrological year",
    #           type = "l",
    #           lwd = 2);
    # abline(h = QMED, lwd = 2, col = "#00A33B")
    # data_list[['Plot']] <- p
    
    if(is.null(rolling_aggregations)){
      stop
    } else {
      Rolling_Aggregations <- data.frame(DateTime = dt$DateTime, Raw = dt$Value)
      for(i in seq_along(rolling_aggregations)){
        window <- rolling_aggregations[i]/interval
        if(rolling_aggregations[i] %% interval > 0){
          cat("Using a rolling aggregation of ", i, "is not divisible by 0.25, skipping for next accumulation\n")
          Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "")] <- rep(NA, length(Rolling_Aggregations[1]))
          next
        } else {
          window <- rolling_aggregations[i]/interval
        }
        
        cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
        Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr", sep = "")] <- roll_max(dt$Value, window, fill = NA)
      }
      data_list[['Rolling_aggs']] <- Rolling_Aggregations
    }
    
  } else if(method == 'min'){
    
    if(interval<1){
      cat("====================== Calculating hourly aggregations =====================\n")
      Hourly <- dt[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(Date, Hour))]
    } else {
      Hourly <- NA
    }
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Min = min(Value, na.rm = TRUE)), Date]
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
    data_list[['Hydro_year']] <- Hydro_year
    
    if(is.null(rolling_aggregations)){
      stop
    } else {
      Rolling_Aggregations <- data.frame(DateTime = dt$DateTime, Raw = dt$Value)
      for(i in seq_along(rolling_aggregations)){
        window <- rolling_aggregations[i]/interval
        if(rolling_aggregations[i] %% interval > 0){
          cat("Using a rolling aggregation of ", i, "is not divisible by 0.25, skipping for next accumulation\n")
          Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "")] <- rep(NA, length(Rolling_Aggregations[1]))
          next
        } else {
          window <- rolling_aggregations[i]/interval
        }
        
        cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
        Rolling_Aggregations[paste("Roll_",rolling_aggregations[i], "hr", sep = "")] <- roll_min(dt$Value, window, fill = NA)
      }
      data_list[['Rolling_aggs']] <- Rolling_Aggregations
    }
    
  } else {
    
    stop("Method has either not been stated or cannot be applied")
    
  }
  class(data_list) <- append(class(data_list)[1:2], c(paste('HydroAggs', method, sep = ''), 'HydroAggs'))
  return(data_list)
}

# Example
Buildwas_Analysis <- hydroAggregate(Buildwas, rolling_aggregations = c(1, 2, 3, 4, 5, 6, 24, 120), method = 'max')


# QMED derivation
QMED.HydroAggsmax <- function(x, ...) {
  QMED_flow<- median(x[['Hydro_year']]$HydroYear_Max)
  QMED <- noquote(paste("Estimated QMED:", QMED_flow, "cumecs"))
  plot(x[['Hydro_year']]$HydrologicalYear, x[['Hydro_year']]$HydroYear_Max,
       xlab = 'Hydrological Year',
       ylab = expression(Flow ~ m^3 ~ s^-1),
       main = 'AMAX flow by hydrological year',
       type = 'l',
       lwd = 2,
       ...)
  abline(h = QMED_flow, lwd = 2, col = '#00A33B')
  return(QMED)
}

QMED<- function(x,...) {
  UseMethod('QMED', x)
}

# Example
QMED(Buildwas_Analysis)

# Summary stats
summary.HydroAggs <- function(x, quantiles = c(0.1, 0.5, 0.7, 0.95), ...) {
  lst <- list()
  
  Flow_perc <- 1- quantiles
  Numeric <-cbind(unlist(sapply(x, lapply, is.numeric)))
  Min <- cbind.data.frame(unlist(sapply(x, lapply, min, na.rm = TRUE)))
  Max <- cbind.data.frame(unlist(sapply(x, lapply, max, na.rm = TRUE)))
  Mean <- suppressWarnings(cbind.data.frame(unlist(sapply(x, lapply, mean, na.rm = TRUE))))
  Median <- cbind.data.frame(unlist(sapply(x, lapply, median, na.rm = TRUE)))
  Quantile <- list()
  for(i in seq_along(quantiles)){
    Quantile[[i]] <- cbind.data.frame(unlist(sapply(x, lapply, function(y) {ifelse(is.numeric(y) == TRUE, quantile(y, probs = Flow_perc[i], na.rm = TRUE), 'FALSE')})))
  }
  Quantile <- do.call('cbind', Quantile)
  df <- cbind.data.frame(Numeric, Min, Max, Mean, Median, Quantile)
  rownames(df) <- as.vector(unlist(lapply(x, colnames)))
  df <- subset(df, Numeric == TRUE)
  df <- subset(df, select = -Numeric)
  delete <- c('Calendar_Year', 'HydrologicalYear')
  df <- df[!(row.names(df) %in% delete),]
  colnames(df) <- c('Min', 'Max', 'Mean', 'Median', paste('Q',quantiles*100, sep = ''))
  
  return(df)
}

summary <- function(x,...) {
  UseMethod('summary', x)
}

# Example
summary(Buildwas_Analysis)


# Month plot
monthplot.HydroAggs <- function(x, name = 'Gauge', polar = FALSE, ...) {
  dt <- x$Monthly
  #dt[, c("Year", "Month") := tstrsplit(Year_Month, " ", fixed=TRUE)]
  dt$Year_Month <- gsub(" ", "-", dt$Year_Month)
  dt$Year_Month <- as.Date(paste(dt$Year_Month,"-01",sep=""))
  p <- ggplot(dt, aes(x = month(Year_Month), y = Monthly_Max, group = year(Year_Month),colour = year(Year_Month))) +
    geom_line(size = 1) +
    xlab("Month") +
    ylab(expression(Flow ~ m^3 ~ s^-1)) +
    ggtitle(paste("Season plot of ", name, sep = "")) +
    labs(colour = 'Year') +
    scale_color_gradient(low = '#D2DE26', high = '#00A33B') +
    scale_x_continuous(breaks = sort(unique(month(dt$Year_Month))), labels = month.abb) +
    theme_light()
  if(polar == TRUE) {
    p <- p + coord_polar()
  }
  return(p)
}

monthplot <- function(x,...) {
  UseMethod('monthplot', x)
}

# Example
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = FALSE)
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = TRUE)

