# Import 15min data - done
#   Data checks - done
#     convert blanks to NAs - done
#   Decide on selection of durations - loops defined by length of vector of values c(1 hr, 3 hr, 6 hr, 24 hr etc.) - done
#   Treat as volumes - to do
#   Peak detection - to do
#   AMAX determination - done
#   QMED - done
#   Single site analysis - to do

if (!require("data.table")) install.packages("data.table") 
library(data.table)
if (!require("RcppRoll")) install.packages("RcppRoll") 
library(RcppRoll)
if (!require("UKFE")) install.packages("UKFE") 
library(UKFE)
if (!require("tidyverse")) install.packages("tidyverse") 
library(tidyverse)
if (!require("beepr")) install.packages("beepr") 
library(beepr)


# Powershell data cleaning
#$flow = Get-Content -Path 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.all' | Select-Object -Skip 16  # Load data
#$flow =  $flow.replace(', ',' ').replace('@ 0.99,', '@ 0.99') # Replace ', ' with ' '
#$flow | Set-Content -Path C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv # Export data

# Functions required
hydroYearDay<-function(d,hy_cal){
  
  # note: this function includes get_hydro_year and should be used instead
  
  # input variables:
  # d: array of dates of class Date
  # hy_cal: hydrological year calendar, current options are 'oct_us_gb', 'sep_br' and 'apr_cl'
  
  if(class(d)!='Date'){stop('d should be of class Date - use as.Date')}
  
  m<-as.numeric(month(d)) # extract month
  y<-as.numeric(year(d)) # extract year
  HydrologicalYear <-y                         # create array for hydrological year
  
  if(hy_cal=='oct_us_gb'){      # USA and Great Britian
    
    HydrologicalYear[m>=10]<-(HydrologicalYear[m>=10]+1)    # hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    start_hy<-as.Date(paste0(HydrologicalYear-1,'-10-01'))
    
  } else if(hy_cal=='sep_br'){  # Brazil
    
    HydrologicalYear[m>=9]<-(HydrologicalYear[m>=9]+1)      # hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    start_hy<-as.Date(paste0(HydrologicalYear-1,'-09-01'))
    
  } else if(hy_cal=='apr_cl'){  # Chile
    
    HydrologicalYear[m<=3]<-(HydrologicalYear[m<=3]-1)      # hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    start_hy<-as.Date(paste0(HydrologicalYear,'-04-01'))
    
  } else {
    
    stop(paste0('Unkown hydrological year calendar:',hy_cal))
    
  }
  
  day_of_hy<-as.numeric(d-start_hy+1) # days since the beginning of the hydro year
  
  if(any(day_of_hy<1|day_of_hy>366)){
    
    stop('Error when computing day of hydro year')
    
  }
  
  return(data.frame(HydrologicalYear,day_of_hy))
  
}
loadAllFlow <- function(file, hydro_year = 'oct_us_gb'){
  dt <- fread(file, 
              #skip = 2,
              na.strings = '---', 
              header = TRUE,
              showProgress = TRUE,
              verbose = TRUE,
              col.names = c('DateTime', 'Value', 'ValueState', 'Runoff', 'RunoffQuality', 'Tags', 'Remarks'))
  
  cat('Removing blank elements\n')
  first_data <- min(which(dt$Value != "NA")) # Locates the first non NA value in the Values field
  dt <- dt[first_data: length(dt$Value)]
  
  cat('Converting dates and times\n')
  dt <- dt[, c("Date", "Time") := tstrsplit(DateTime, " ", fixed=TRUE)]
  dt$Date <- as.Date(dt$Date, format = '%d/%m/%Y')
  dt$DateTime <- as.POSIXct(dt$DateTime, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
  dt$Hour <- as.numeric(substr(dt$Time, 1, 2))
  
  # Hydrological day and year
  cat("Calculating hydrological year and day\n")
  hydro_year <- 'oct_us_gb'
  hydroData <- hydroYearDay(dt$Date, hy_cal = hydro_year)
  
  # Merge 2 tables
  cat("Compilation complete\n")
  dt_1 <- data.table(dt, hydroData)
  class(dt_1) <- append(class(dt_1), "FlowLoad")
  return(dt_1)
}
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
    class(Hourly) <- append(class(Hourly)[1:2], paste('Hourly', method, sep = ''))
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Mean = mean(Value, na.rm = TRUE)), Date]
    class(Daily) <- append(class(Daily)[1:2], paste('Daily', method, sep = ''))
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    class(Monthly) <- append(class(Monthly)[1:2], paste('Monthly', method, sep = ''))
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    class(Monthly) <- append(class(Monthly)[1:2], paste('Monthly', method, sep = ''))
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
    class(Hydro_year) <- append(class(Hydro_year)[1:2], paste('Hydro_year', method, sep = ''))
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
    class(Hourly) <- append(class(Hourly)[1:2], paste('Hourly', method, sep = ''))
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Sum = sum(Value, na.rm = TRUE)), Date]
    class(Daily) <- append(class(Daily)[1:2], paste('Daily', method, sep = ''))
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Sum = sum(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    class(Monthly) <- append(class(Monthly)[1:2], paste('Monthly', method, sep = ''))
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Sum = sum(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    class(Annual) <- append(class(Annual)[1:2], paste('Annual', method, sep = ''))
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Sum = sum(Value, na.rm = TRUE)), HydrologicalYear]
    class(Hydro_year) <- append(class(Hydro_year)[1:2], paste('Hydro_year', method, sep = ''))
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
    class(Hourly) <- append(class(Hourly)[1:2], paste('Hourly', method, sep = ''))
    data_list[['Hourly']] <- Hourly
    
    cat("====================== Calculating daily aggregations ======================\n")
    Daily <- dt[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = Date)]
    class(Daily) <- append(class(Daily)[1:2], paste('Daily', method, sep = ''))
    data_list[['Daily']] <- Daily
    
    cat("====================== Calculating monthly aggregations ====================\n")
    Monthly <- dt[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(Date), month(Date)))]
    class(Monthly) <- append(class(Monthly)[1:2], paste('Monthly', method, sep = ''))
    data_list[['Monthly']] <- Monthly
    
    cat("====================== Calculating annual aggregations =====================\n")
    Annual <- dt[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(Date))]
    class(Annual) <- append(class(Annual)[1:2], paste('Annual', method, sep = ''))
    data_list[['Annual']] <- Annual
    
    cat("====================== Calculating Hydro Year aggregations =================\n")
    Hydro_year <- dt[, .(HydroYear_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
    class(Hydro_year) <- append(class(Hydro_year)[1:2], paste('Hydro_year', method, sep = ''))
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
  class(data_list) <- append(class(data_list), c(paste('HydroAggs', method, sep = ''), 'HydroAggs'))
  return(data_list)
}

# QMED derivation for HydroAggsmax objects
QMEDPlot.HydroAggsmax <- function(x, ...) {
  QMED_flow<- median(x$Hydro_year$HydroYear_Max, na.rm = TRUE)
  QMED <- noquote(paste("Estimated QMED:", QMED_flow, "cumecs"))
  p <- ggplot(x$Hydro_year, aes(x = HydrologicalYear, y = HydroYear_Max)) +
    geom_line(size = 1.2) +
    xlab('Hydrological Year') +
    ylab(expression(Flow ~ m^3 ~ s^-1)) +
    ggtitle('AMAX flow by hydrological year') +
    geom_hline(yintercept = QMED_flow, colour = '#00A33B', size = 2)
  beepr::beep(sound = sample(1:11, 1), expr = "WOW")
  print(QMED)
  p
}
QMEDPlot<- function(x,...) {
  UseMethod('QMEDPlot', x)
}

# Summary stats for any HydroAggs objects
summary.HydroAggs <- function(x, quantiles = c(0.1, 0.5, 0.7, 0.95), ...) {
  Flow_perc <- 1- quantiles
  Numeric <-cbind(unlist(sapply(x, lapply, is.numeric)))
  Min <- cbind(unlist(sapply(x, lapply, min, na.rm = TRUE)))
  Max <- cbind(unlist(sapply(x, lapply, max, na.rm = TRUE)))
  Mean <- cbind(unlist(sapply(x, lapply, function(y) {ifelse(is.numeric(y) == TRUE, round(mean(y, na.rm = TRUE), 3), 'FALSE')})))
  Median <-cbind(unlist(sapply(x, lapply, function(y) {ifelse(is.numeric(y) == TRUE, median(y, na.rm = TRUE), 'FALSE')})))
  Quantile <- list()
  for(i in seq_along(quantiles)){
    Quantile[[i]] <- as.data.frame(cbind(unlist(sapply(x, lapply, function(y) {ifelse(is.numeric(y) == TRUE, quantile(y, probs = Flow_perc[i], na.rm = TRUE), 'FALSE')}))))
  }
  Quantile <- do.call('cbind', Quantile)
  df <- cbind(Numeric, Min, Max, Mean, Median, Quantile)
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

# Month plot
monthplot.HydroAggs <- function(x, name = 'Gauge', polar = FALSE, snip = NULL, ...) {
  dt <- x$Monthly
  #dt[, c("Year", "Month") := tstrsplit(Year_Month, " ", fixed=TRUE)]
  dt$Year_Month <- gsub(" ", "-", dt$Year_Month)
  dt$Year_Month <- as.Date(paste(dt$Year_Month,"-01",sep=""))
  dt <- headTail(dt, n = snip)
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
singleSite <- function(Date, AMAX, returns = c(2,5,15,20,50,75,100,200,1000), fit = 'all', flows = NULL, trend = FALSE) {
  data <- list()
  if(fit == 'all'){
    # Growth curve plots
    par(mfrow = c(2,2))
    EVPlot1(AMAX, dist = "GEV", 
            Title = 'Generalised Extreme Value')
    EVPlot1(AMAX, dist = "GenPareto", 
            Title = 'Generalised Pareto')
    EVPlot1(AMAX, dist = "Gumbel", 
            Title = 'Gumbel')
    EVPlot1(AMAX, dist = "GenLog", 
            Title = 'Generalised Logistic')
    par(mfrow = c(1,1))
    
    # Growth curve parameters
    param <- c('Loc', 'Scale','Shape')
    gev <- GEVPars(AMAX)
    genP <- GenParetoPars(AMAX)
    gum <- GumbelPars(AMAX)
    gum$Shape <- NA
    genL <- GenLogPars(AMAX)
    
    Shapes <- data.frame(GEV = t(gev), 
                         GenPareto = t(gum),
                         Gumbel = t(genP),
                         GenLogistic = t(genL))
    
    # Model Fits
    gev_fit <- GoTF1(AMAX, dist = "GEV")
    genP_fit <- GoTF1(AMAX, dist = "GenPareto")
    gum_fit <- GoTF1(AMAX, dist = "Gumbel")
    genL_fit <- GoTF1(AMAX, dist = "GenLog")
    
    fits <- data.frame(GEV = t(gev_fit), 
                       GenPareto = t(genP_fit), 
                       Gumbel = t(gum_fit),
                       GenLogistic = t(genL_fit))
    
    # Growth Factors
    gev_gf <- GEVGF(Lcv(AMAX), LSkew(AMAX), RP = returns)
    genP_gf <- GenParetoGF(Lcv(AMAX), LSkew(AMAX), ppy = 1, RP = returns)
    gum_gf <- GumbelGF(Lcv(AMAX), RP = returns)
    genL_gf <- GenLogGF(Lcv(AMAX), LSkew(AMAX), RP = returns)
    
    gf <- data.frame(Return_period = returns,
                     GEV = gev_gf,
                     GenPareto = genP_gf,
                     Gumbel = gum_gf,
                     GenLogistic = genL_gf)
    
    # Flows by return period
    GEV_flows <- GEVAM(AMAX, RP = returns, q = NULL, trend = trend)
    GenPareto_flows <- GenParetoPOT(AMAX, ppy = 1, RP = returns, q = NULL)
    Gumbel_flows <- GumbelAM(AMAX, RP = returns, q = NULL, trend = trend)
    GenLog_flows <- GenLogAM(AMAX, RP = returns, q = NULL, trend = trend)
    
    Flows <- data.frame(Return_Period = returns, 
                        GEV = GEV_flows, 
                        GenPareto = GenPareto_flows, 
                        Gumbel = Gumbel_flows,
                        GenLogistic = GenLog_flows)
    
    data[['Model_Shapes']] <- Shapes
    data[['Model_Fits']] <- fits
    data[['Growth_Factors']] <- gf
    data[['Estimated_Flows']] <- Flows
    
    
    # Estimated return period of user specified flow
    if(is.null(flows)){
      stop
    } else{
      
      GEV_rp <- GEVAM(AMAX, RP = NULL, q = flows, trend = trend)
      Gumbel_rp <- GumbelAM(AMAX, RP = NULL, q = flows, trend = trend)
      GenPareto_rp <- GenParetoPOT(AMAX, ppy = 1, RP = NULL, q = flows)
      GenLog_rp <- GenLogAM(AMAX, RP = NULL, q = flows, trend = trend)
      
      RPs <- data.frame(Sample_Flows = flows, 
                        GEV = GEV_rp, 
                        Gumbel = Gumbel_rp,
                        GenPareto = GenPareto_rp, 
                        GenLogistic = GenLog_rp)
      
      data[['Estimated_RP']] <- RPs
    }
    
    return(data)
    
  }
}

# The following are adapted from the UKFE package - fixes the Gumbel issues
GumbelGF <- function(lcv, RP){
  B <- lcv/(log(2)-lcv*(0.5772+log(log(2))))
  gf <- 1+B*(log(log(2))-log(-log(1-(1/RP))))
  return(gf)
}
GumbelEst <- function(loc, scale, q = NULL,RP = 100){
  if(is.null(q) == TRUE) {res <- loc+scale*(-log(-log(1-(1/RP))))}
  else {
    Prob <- 1- exp(-exp(-(q - loc)/scale))
    res <- 1/Prob}
  return(res)
}
GumbelPars <- function(x = NULL, mle = FALSE, L1, LCV){
  if(is.null(x) == FALSE & is.numeric(x) == FALSE) {stop("x must be a numeric vector")}
  if(mle == FALSE) {
    if(is.null(x)) {Scale <- (L1*LCV)/log(2)
    Loc <- L1 - 0.5772*Scale
    return(data.frame(Loc, Scale))
    } else {
      L1 <- mean(x, na.rm = TRUE)
      LCV <- Lcv(x)
      Scale <- (L1*LCV)/log(2)
      Loc <- L1 - 0.5772*Scale
      return(data.frame(Loc, Scale))
    }
  } else {
    pars <- c(mean(x), sd(x)/1.5)
    max.lhd <- function(q, par) {
      abs(sum(log(gum.pdf(q, loc = par[1], scale = par[2]))))
    }
    gum.pdf <- function(q, loc, scale) {
      p <- scale^-1 * exp(-(q-loc)/scale)*exp(-exp(-(q-loc)/scale))
      return(p)
    }
    result <- suppressWarnings(optim(par = pars, fn = max.lhd, q = x))
    loc <- result$par[1]
    scale <- result$par[2]
    log.likelihood <- -result$value[1]
    message <- result$message
    Res <- data.frame(loc, scale, log.likelihood)
    return(Res)
  }
}
GumbelAM <- function(x, RP = 100, q = NULL, trend = FALSE){
  if(is.numeric(x) == FALSE) {stop("x must be a numeric vector")}
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  a <- L2/log(2)
  loc <- L1 - 0.5772*a
  if(is.null(q) == TRUE) {res <- loc - a * log(-log(1-(1/RP)))}
  else {
    Prob <- 1- exp(-exp(-(q - loc)/a))
    res <- 1/Prob
  }
  m <- function(i, j) {sum((i-mean(i))*(j-mean(j)))/sum((i-mean(i))^2)}
  M <- m(i = seq(1, length(x)), j = x)
  b <- mean(x)-M*mean(seq(1,length(x)))
  LM <- (M*(length(x))+b)-(M*median(seq(1,length(x)))+b)
  LocTrend <- loc+LM
  if (is.null(q) == TRUE) {resTrend <- LocTrend - a * log(-log(1-(1/RP)))}
  else {
    ProbTrend <- 1- exp(-exp(-(q - LocTrend)/a))
    resTrend <- 1/ProbTrend}
  if(trend == FALSE) {return(res)} else {return(resTrend)}
}
GoTF1 <- function(x, dist = "GenLog", pars = NULL, GF = NULL, RepDist = NULL){
  if(is.numeric(x) == FALSE) {stop("x must be a numeric vector")}
  if(is.null(RepDist) == FALSE) {
    if(length(RepDist) != (5000*length(x))) {print("Warning: RepDist not equal to 5000 * length(x), resampling has been used")}
    if(length(RepDist) != (5000*length(x))) {RepDist <- sample(RepDist, 5000*length(x), replace = TRUE)}
    MMR <- function(x) {sd(x[x > quantile(x, 0.75, na.rm = TRUE)])/mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Mat.1 <- matrix(RepDist, nrow = length(x), ncol = 5000)
    MMRs <- apply(Mat.1, 2, MMR)
    MMRo <- MMR(x)
    SortMMRs <- sort(MMRs)
    Ind <- suppressWarnings(min(which(SortMMRs >  MMRo)))
    if(Ind == Inf | Ind == 0) {Prop <- 0.0002} else {Prop <- Ind/5000}
    if(Prop > 0.5) {res <- 1-Prop} else {res <- Prop}
    if(Ind == Inf | res == 0) {res <- "< 0.0002"} else {res <- res/0.5}
    
    TailMean <- function(x) {mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Mat.2 <- matrix(RepDist, nrow = length(x), ncol = 5000)
    TMs <- apply(Mat.1, 2, TailMean)
    TMo <- TailMean(x)
    SortTMs <- sort(TMs)
    Ind2 <- suppressWarnings(min(which(SortTMs >  TMo)))
    if(Ind2 == Inf | Ind2 == 0) {Prop2 <- 0.0002} else {Prop2 <- Ind2/5000}
    if(Prop2 > 0.5) {res2 <- 1-Prop2} else {res2 <- Prop2}
    if(Ind2 == Inf | res2 == 0) {res2 <- "< 0.0002"} else {res2 <- res2/0.5}
    
    ResDF <- data.frame(res, res2)
    colnames(ResDF) <- c("p(Tail cv)", "p(Tail mean)")
    return(ResDF)
    
  } else {
    
    if(dist == "GenLog") {funcX <- GenLogAM
    funcPars <- GenLogEst
    funcGF <- GenLogGF}
    if(dist == "GEV")
    {funcX <- GEVAM
    funcPars <- GEVEst
    funcGF <- GEVGF}
    if(dist == "Gumbel")
    {funcX <- GumbelAM
    funcPars <- GumbelEst
    funcGF <- GumbelGF}
    if(dist == "GenPareto")
    {funcX <- GenParetoPOT
    funcPars <- GenParetoEst
    funcGF <- GenParetoGF}
    MMR <- function(x) {sd(x[x > quantile(x, 0.75, na.rm = TRUE)])/mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Rands <- 1/runif(length(x)*5000)
    if(is.null(pars) == TRUE & is.null(GF) == TRUE) {Sims <- funcX(x, RP = Rands)}
    if(is.null(pars) == FALSE) {
      if(dist == "Gumbel") {Sims <- funcPars(pars[1], pars[2], RP = Rands)} else
      {Sims <- funcPars(pars[1], pars[2], pars[3], RP = Rands)}}
    if(is.null(GF) == FALSE)  {
      if(dist == "Gumbel") {Sims <- funcGF(GF[1], RP = Rands)*GF[3]} else
      {Sims <- funcGF(GF[1], GF[2], RP = Rands)*GF[3]}}
    
    Mat.1 <- matrix(Sims, nrow = length(x), ncol = 5000)
    MMRs <- apply(Mat.1, 2, MMR)
    MMRo <- MMR(x)
    SortMMRs <- sort(MMRs)
    Ind <- suppressWarnings(min(which(SortMMRs >  MMRo)))
    if(Ind == Inf | Ind == 0) {Prop <- 0.0002} else {Prop <- Ind/5000}
    if(Prop > 0.5) {res <- 1-Prop} else {res <- Prop}
    if(Ind == Inf | res == 0) {res <- "< 0.0002"} else {res <- res/0.5}
    
    TailMean <- function(x) {mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Mat.2 <- matrix(Sims, nrow = length(x), ncol = 5000)
    TMs <- apply(Mat.1, 2, TailMean)
    TMo <- TailMean(x)
    SortTMs <- sort(TMs)
    Ind2 <- suppressWarnings(min(which(SortTMs >  TMo)))
    if(Ind2 == Inf | Ind2 == 0) {Prop2 <- 0.0002} else {Prop2 <- Ind2/5000}
    if(Prop2 > 0.5) {res2 <- 1-Prop2} else {res2 <- Prop2}
    if(Ind2 == Inf | res2 == 0) {res2 <- "< 0.0002"} else {res2 <- res2/0.5}
    if(class(res) == "character") {res <- res} else {res <- round(res, 4)}
    if(class(res2) == "character") {res2 <- res2} else {res2 <- round(res2, 4)}
    ResDF <- data.frame(res, res2)
    colnames(ResDF) <- c("p(Tail cv)", "p(Tail mean)")
    return(ResDF)
    
  }
}
EVPlot1 <- function(x, dist = "GenLog", scaled = TRUE, Title = "Extreme value plot", ylabel = NULL, LineName = NULL, Unc = TRUE) {
  if(class(x) != "numeric") stop ("x must be a numeric vector")
  if(dist == "GenLog") {func <- GenLogGF}
  if(dist == "GEV") {func <- GEVGF}
  if(dist == "GenPareto") {func <- GenParetoGF}
  if(dist == "Gumbel") {func <- GumbelGF}
  Ranks <- seq(500, 1)
  Gringorten <- function(Rank, n) {(Rank-0.44)/(n+0.12)}
  Gring <- Gringorten(Ranks, 500)
  Log.Red.Var <- log((1/Gring)-1)
  RPs <- 1/Gring
  Ranks.obs <- seq(length(x), 1)
  Gring.obs <- Gringorten(Ranks.obs, length(x))
  LRV.obs <- log((1/Gring.obs)-1)
  Scale <- x/median(x)
  if(scaled == TRUE) {AM.sort <- sort(Scale, decreasing = F)} else {AM.sort <- sort(x, decreasing = F)}
  ss.lcv <- UKFE::Lcv(x)
  ss.lskew <- UKFE::LSkew(x)
  if(scaled == TRUE) {
    if(dist == "Gumbel") {SimSS <- func(ss.lcv, RP = 1/Gring)} else {
      SimSS <- func(ss.lcv, ss.lskew, RP = 1/Gring)}} else {
        if(dist == "Gumbel") {SimSS <- func(ss.lcv, RP = 1/Gring)*median(x)} else
        {SimSS <- func(ss.lcv, ss.lskew, RP = 1/Gring)*median(x)}}
  if(is.null(ylabel) == TRUE) {
    if(scaled == TRUE) {YLab <- "Q/QMED"} else {YLab <- "Discharge (m3/s)"}} else {YLab = ylabel}
  Ymax <- median(c(max(AM.sort), max(SimSS)))
  UpperYRange <- (Ymax-median(AM.sort))
  UpperObsRange <- (max(AM.sort)-median(AM.sort))
  LowerYRange <- median(AM.sort)-min(AM.sort)
  ymin <- median(AM.sort)-(UpperObsRange)
  if(Ymax < max(AM.sort)) {Ymax <- max(AM.sort)} else {Ymax <- Ymax}
  if(LowerYRange > 0.143*UpperYRange) {ymin <- min(AM.sort)} else {ymin <- ymin}
  
  plot(Log.Red.Var, SimSS, type = "l", xlim = c(min(LRV.obs),7), ylim = c(ymin, Ymax), main = Title, ylab = YLab, xlab = "logistic reduced variate", lwd = 2)
  points(LRV.obs, AM.sort, col = "blue", lwd = 1.5)
  if(Unc == FALSE) {
    if(is.null(LineName) == TRUE) {
      if(scaled == FALSE) {legend("topleft", legend = c("Frequency curve", "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)} else {legend("topleft", legend = c("Growth curve", "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
    } else {legend("topleft", legend = c(LineName, "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
  } else {
    if(is.null(LineName) == TRUE) {
      if(scaled == FALSE) {legend("topleft", legend = c("Frequency curve", "Observed", "95% Intervals"), col = c("black", "blue", "black"), lty = c(1,0,3), pch = c(NA, 1, NA), bty = "n", lwd = c(2,NA,2), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)} else {legend("topleft", legend = c("Growth curve", "Observed", "95% Intervals"), col = c("black", "blue", "black"), lty = c(1,0,3), pch = c(NA, 1, NA), bty = "n", lwd = c(2,NA,2), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
    } else {legend("topleft", legend = c(LineName, "Observed", "95% Intervals"), col = c("black", "blue", "black"), lty = c(1,0,3), pch = c(NA, 1, NA), bty = "n", lwd = c(2,NA,2), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
  }
  T.Plot.Lab <- c(2,5,10,20,50,100, 500)
  At <- log(T.Plot.Lab-1)
  AxisPos <- median(c(ymin, median(SimSS)))
  axis(side = 1, at = At, pos = AxisPos, lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
  TextY <- as.numeric(quantile(seq(ymin, median(SimSS), by = abs(ymin/10)), 0.86))
  text(2, TextY, labels = "Return Period (yrs)", cex = 0.75, pos = 4)
  abline(v = 0, lty = 3)
  if(scaled == TRUE) {abline(h = 1, lty = 3)} else {abline(h = median(x), lty = 3)}
  if(Unc == TRUE){
    resample <- sample(x, size = length(x)*500, replace = TRUE)
    mat <- matrix(resample, nrow = length(x), ncol = 500)
    Medians <- apply(mat, 2, median)
    LmomsAll <- Lmoms(mat[,1])
    for(i in 2:500) {LmomsAll <- rbind(LmomsAll, Lmoms(mat[,i]))}
    if(dist == "Gumbel") {FCs <- func(LmomsAll$Lcv[1], RP = RPs)*Medians[1]
    for(i in 2:500) {FCs <- rbind(FCs, func(LmomsAll$Lcv[i], RP = RPs)*Medians[i])} } else {
      FCs <- func(LmomsAll$Lcv[1], LmomsAll$LSkew[1], RP = RPs)*Medians[1]
      for(i in 2:500) {FCs <- rbind(FCs, func(LmomsAll$Lcv[i], LmomsAll$LSkew[i], RP = RPs)*Medians[i])}}
    lower95 <- as.numeric(apply(FCs, 2, quantile, 0.025, na.rm = TRUE))
    upper95 <- as.numeric(apply(FCs, 2, quantile, 0.975, na.rm = TRUE))
    if(scaled == TRUE) {
      lower95 <- lower95/median(x)
      upper95 <- upper95/median(x)
    }
    points(Log.Red.Var, lower95, type = "l", lty = 3, lwd = 2)
    points(Log.Red.Var, upper95, type = "l", lty = 3, lwd = 2)
    
  }
}


#Test the functions
# Load Buildwas data in

#link <-'O:/National Modelling and Forecasting/21_Strategic Delivery/Flood Forecast Modelling/03 Training/HydroER_package/Aggregations/Buildwas_15min_Flow.csv'
link <- 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv'
Buildwas <- loadAllFlow(link)
Buildwas_Analysis <- hydroAggregate(Buildwas, rolling_aggregations = c(1,2,3,4,5,6,24, 120), method = 'max')
QMEDPlot(Buildwas_Analysis)
summary(Buildwas_Analysis)
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = FALSE)
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = TRUE)
singleSite(Buildwas_Analysis[[5]]$HydrologicalYear, Buildwas_Analysis[[5]]$HydroYear_Max, flows = c(300,500, 980))
#beepr::beep(sound = 3, expr = NULL)

#UncSS(Buildwas_Analysis[[5]]$Max, func = GEVAM, RP = c(2,4,5,10,25,50, 75, 100, 200))
