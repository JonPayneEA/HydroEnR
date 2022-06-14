HydroEnR
================
Jonathan Payne
14/06/2022

`{r, echo = FALSE} knitr::opts_chunk$set( fig.path =
"README_figs/README-" )`

# Welcome to HydroEnR

This package is designed for members of Evidence and Risk. It allows you
to;

  - Easily load data from WISKI
      - Carry out quality checks on these data  
  - Aggregate data by different periods and through various functions
    using rapid C++ elements
  - Detect peaks in hydrological data
  - Convert flow data to a volumetric measure over a designated time
    step
  - Develop Thiessen polygons for use in our realtime flood forecast
    models
  - Download up-to-date meta data on EA gauges
  - Import recent flow, stage and rain gauge data via the EAs API
  - Detect trends in hydrological data
  - Inspect ARMA parameters for stability in a real time forecasting
    context
  - Use basic hydraulic equations such as the Mannings’ equation
      - Import cross sectional data and carry out rudimentry analyses
  - Carry out single site analyses
      - ESS in the near future
  - Use a greater selection of objective functions during model
    calibration

\`\`\`{r,message=FALSE, warning=FALSE, echo = FALSE, results = FALSE}
library(data.table) library(RcppRoll) library(ggplot2)
hydroYearDay\<-function(d,hy\_cal){

\# note: this function includes get\_hydro\_year and should be used
instead

\# input variables: \# d: array of dates of class Date \# hy\_cal:
hydrological year calendar, current options are ‘oct\_us\_gb’, ‘sep\_br’
and ‘apr\_cl’

if(class(d)\!=‘Date’){stop(‘d should be of class Date - use as.Date’)}

m\<-as.numeric(month(d)) \# extract month y\<-as.numeric(year(d)) \#
extract year HydrologicalYear \<-y \# create array for hydrological year

if(hy\_cal==‘oct\_us\_gb’){ \# USA and Great Britian

    HydrologicalYear[m>=10]<-(HydrologicalYear[m>=10]+1)    # hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    start_hy<-as.Date(paste0(HydrologicalYear-1,'-10-01'))

} else if(hy\_cal==‘sep\_br’){ \# Brazil

    HydrologicalYear[m>=9]<-(HydrologicalYear[m>=9]+1)      # hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    start_hy<-as.Date(paste0(HydrologicalYear-1,'-09-01'))

} else if(hy\_cal==‘apr\_cl’){ \# Chile

    HydrologicalYear[m<=3]<-(HydrologicalYear[m<=3]-1)      # hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    start_hy<-as.Date(paste0(HydrologicalYear,'-04-01'))

} else {

    stop(paste0('Unkown hydrological year calendar:',hy_cal))

}

day\_of\_hy\<-as.numeric(d-start\_hy+1) \# days since the beginning of
the hydro year

if(any(day\_of\_hy\<1|day\_of\_hy\>366)){

    stop('Error when computing day of hydro year')

}

return(data.frame(HydrologicalYear,day\_of\_hy))

} loadAllFlow \<- function(file, hydro\_year = ‘oct\_us\_gb’){ dt \<-
fread(file, \#skip = 2, na.strings = ‘—’, header = TRUE, showProgress =
TRUE, verbose = TRUE, col.names = c(‘DateTime’, ‘Value’, ‘ValueState’,
‘Runoff’, ‘RunoffQuality’, ‘Tags’, ‘Remarks’))

cat(‘Removing blank elements’) first\_data \<-
min(which(dt\(Value != "NA")) # Locates the first non NA value in the Values field  dt <- dt[first_data: length(dt\)Value)\]

cat(‘Converting dates and times’) dt \<- dt\[, c(“Date”, “Time”) :=
tstrsplit(DateTime, " ", fixed=TRUE)\] dt\(Date <- as.Date(dt\)Date,
format = ‘%d/%m/%Y’) dt\(DateTime <- as.POSIXct(dt\)DateTime, format =
“%d/%m/%Y %H:%M:%S”, tz = “GMT”)
dt\(Hour <- as.numeric(substr(dt\)Time, 1, 2))

\# Hydrological day and year cat(“Calculating hydrological year and
day”) hydro\_year \<- ‘oct\_us\_gb’ hydroData \<-
hydroYearDay(dt$Date, hy\_cal = hydro\_year)

\# Merge 2 tables cat(“Compilation complete”) dt\_1 \<- data.table(dt,
hydroData) class(dt\_1) \<- append(class(dt\_1), “FlowLoad”)
return(dt\_1) }

# Dim version that can handle 1D vectors

dimC \<- function(item) { if (is.null(base::dim(item)) ) {
dims\<-length(item)  
} else{ dims \<- base::dim(item)  
} return(dims) }

headTail \<- function(x, n = NULL){ \# print the head and tail together
if(is.null(n)){ n \<- dimC(x)\[1\] } if(n \< 0){ dt \<-head(x,n\*-1) }
else { dt \<- tail(x, n) } return(dt) }

hourlyAgg.FlowLoad \<- function(x, method = mean, …){ if(method ==
‘mean’) { Hourly \<- x\[, .(Hourly\_Mean = mean(Value, na.rm =
TRUE)), .(Hourly = paste(Date, Hour))\] } if(method == ‘median’) {
Hourly \<- x\[, .(Hourly\_Median = median(Value, na.rm = TRUE)),
.(Hourly = paste(Date, Hour))\] } if(method == ‘min’) { Hourly \<- x\[,
.(Hourly\_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(Date,
Hour))\] } if(method == ‘max’) { Hourly \<- x\[, .(Hourly\_Max =
max(Value, na.rm = TRUE)), .(Hourly = paste(Date, Hour))\] }
return(Hourly) } hourlyAgg \<- function(x, method = ‘mean’, …) {
UseMethod(‘hourlyAgg’, x) }

# Daily aggregation

dailyAgg.FlowLoad \<- function(x, method = mean, …){ if(method ==
‘mean’) { Daily \<- x\[, .(Daily\_Mean = mean(Value, na.rm = TRUE)),
Date\] } if(method == ‘median’) { Daily \<- x\[, .(Daily\_Median =
median(Value, na.rm = TRUE)), Date\] } if(method == ‘min’) { Daily \<-
x\[, .(Daily\_Min = min(Value, na.rm = TRUE)), Date\] } if(method ==
‘max’) { Daily \<- x\[, .(Daily\_Max = max(Value, na.rm = TRUE)),
Date\] } return(Daily) } dailyAgg \<- function(x, method = ‘mean’, …) {
UseMethod(‘dailyAgg’, x) }

# Monthly aggregation

monthlyAgg.FlowLoad \<- function(x, method = mean, …){ if(method ==
‘mean’) { Monthly \<- x\[, .(Monthly\_Mean = mean(Value, na.rm =
TRUE)), .(Year\_Month = paste(year(Date), month(Date)))\] } if(method ==
‘median’) { Monthly \<- x\[, .(Monthly\_Median = median(Value, na.rm =
TRUE)), .(Year\_Month = paste(year(Date), month(Date)))\] } if(method ==
‘min’) { Monthly \<- x\[, .(Monthly\_Min = min(Value, na.rm = TRUE)),
.(Year\_Month = paste(year(Date), month(Date)))\] } if(method == ‘max’)
{ Monthly \<- x\[, .(Monthly\_Max = max(Value, na.rm = TRUE)),
.(Year\_Month = paste(year(Date), month(Date)))\] } return(Monthly) }
monthlyAgg \<- function(x, method = ‘mean’, …) { UseMethod(‘monthlyAgg’,
x) }

# Annual aggregation

annualAgg.FlowLoad \<- function(x, method = mean, …){ if(method ==
‘mean’) { Annual \<- x\[, .(Annual\_Mean = mean(Value, na.rm =
TRUE)), .(Calendar\_Year = year(Date))\] } if(method == ‘median’) {
Annual \<- x\[, .(Monthly\_Median = median(Value, na.rm = TRUE)),
.(Calendar\_Year = year(Date))\] } if(method == ‘min’) { Annual \<- x\[,
.(Annual\_Min = min(Value, na.rm = TRUE)), .(Calendar\_Year =
year(Date))\] } if(method == ‘max’) { Annual \<- x\[, .(Annual\_Max =
max(Value, na.rm = TRUE)), .(Calendar\_Year = year(Date))\] }
return(Annual) } annualAgg \<- function(x, method = ‘mean’, …) {
UseMethod(‘annualAgg’, x) }

# Hydrological year aggregation

hydroYearAgg.FlowLoad \<- function(x, method = mean, …){ if(method ==
‘mean’) { Hydro\_year \<- x\[, .(Hydro\_year\_Mean = mean(Value, na.rm
= TRUE)), HydrologicalYear\] } if(method == ‘median’) { Hydro\_year \<-
x\[, .(Monthly\_Median = median(Value, na.rm = TRUE)),
HydrologicalYear\] } if(method == ‘min’) { Hydro\_year \<- x\[,
.(Hydro\_year\_Min = min(Value, na.rm = TRUE)), HydrologicalYear\] }
if(method == ‘max’) { Hydro\_year \<- x\[, .(Hydro\_year\_Max =
max(Value, na.rm = TRUE)), HydrologicalYear\] } return(Hydro\_year) }
hydroYearAgg \<- function(x, method = ‘mean’, …) {
UseMethod(‘hydroYearAgg’, x) }

# Rolling Aggregations

rollingAggs.FlowLoad \<- function(dt, rolling\_aggregations = c(1, 2, 3,
4, 8, 24, 120), interval = 0.25, method = ‘mean’){ roller \<-
get(paste0(“roll\_”, method)) agg \<- length(rolling\_aggregations)
Rolling\_Aggregations \<- data.table(DateTime =
dt\(DateTime, Raw = dt\)Value) for(i in
seq\_along(rolling\_aggregations)){ window \<-
rolling\_aggregations\[i\]/interval if(rolling\_aggregations\[i\] %%
interval \> 0){ cat(“Using a rolling aggregation of”,
rolling\_aggregations\[i\], “is not divisible by 0.25, skipping for next
accumulation”) Rolling\_Aggregations\[,
paste(“Roll\_”,rolling\_aggregations\[i\], “hr\_”, method, sep = "")
:= rep(NA, length(Rolling\_Aggregations$DateTime))\] next } else {
window \<- rolling\_aggregations\[i\]/interval }

    cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
    Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):=roller(Rolling_Aggregations$Raw, window, fill = NA)] 

} return(Rolling\_Aggregations) } rollingAggs \<- function(dt,
rolling\_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25,
method = ‘mean’) { UseMethod(‘rollingAggs’, dt) }

hydroAggregate \<- function(dt, interval = 0.25, rolling\_aggregations =
c(1, 2, 3, 4, 8, 24, 120), method = ‘mean’) { if(missingArg(dt)){
stop(“Data missing. Please supply data to run this function”) }
if(class(Buildwas)\[1\] \!= “data.table”){ stop(“Please supply data as a
data.table. Try loading data into R using loadAllFlow() to get correct
format for analysis.”) } if(“Value” %in% colnames(dt) == FALSE){
stop(“Values (flow) field missing from data.table”) } if(“DateTime”
%in% colnames(dt) == FALSE){ stop(“DateTime field missing from
data.table”) } if(“Date” %in% colnames(dt) == FALSE){ stop(“Date field
missing from data.table”) } if(“Hour” %in% colnames(dt) == FALSE){
stop(“Hour field missing from data.table”) }

data\_list \<- list() if(interval\<1) { cat(“======================
Calculating hourly aggregations =====================”) Hourly \<-
hourlyAgg(dt, method = method) } else { Hourly \<- NA }
data\_list\[\[‘Hourly’\]\] \<- Hourly

cat(“====================== Calculating daily aggregations
======================”) Daily \<- dailyAgg(dt, method = method)
data\_list\[\[‘Daily’\]\] \<- Daily

cat(“====================== Calculating monthly aggregations
====================”) Monthly \<- monthlyAgg(dt, method = method)
data\_list\[\[‘Monthly’\]\] \<- Monthly

cat(“====================== Calculating annual aggregations
=====================”) Annual \<- annualAgg(dt, method = method)
data\_list\[\[‘Annual’\]\] \<- Annual

cat(“====================== Calculating Hydro Year aggregations
=================”) Hydro\_year \<- hydroYearAgg(dt, method = method)
data\_list\[\[‘Hydro\_year’\]\] \<- Hydro\_year

if(length(rolling\_aggregations) \> 0){ Rolling\_Aggregations \<-
rollingAggs(dt, interval = interval, rolling\_aggregations =
rolling\_aggregations, method = method) }
data\_list\[\[‘Rolling\_Aggregations’\]\] \<- Rolling\_Aggregations
class(data\_list) \<- append(class(data\_list), c(paste(‘HydroAggs’,
method, sep = ’‘), ’HydroAggs’)) return(data\_list) }

monthplot.HydroAggs \<- function(x, name = ‘Gauge’, polar = FALSE, snip
= NULL, …) { dt \<-
x\(Monthly  #dt[, c("Year", "Month") := tstrsplit(Year_Month, " ", fixed=TRUE)]  dt\)Year\_Month
\<- gsub(" “,”-", dt\(Year_Month)  dt\)Year\_Month \<-
as.Date(paste(dt\(Year_Month,"-01",sep=""))  dt <- headTail(dt, n = snip)  p <- ggplot(dt, aes(x = month(Year_Month), y = Monthly_Max, group = year(Year_Month),colour = year(Year_Month))) +  geom_line(size = 1) +  xlab("Month") +  ylab(expression(Flow ~ m^3 ~ s^-1)) +  ggtitle(paste("Season plot of ", name, sep = "")) +  labs(colour = 'Year') +  scale_color_gradient(low = '#D2DE26', high = '#00A33B') +  scale_x_continuous(breaks = sort(unique(month(dt\)Year\_Month))),
labels = month.abb) + theme\_light() if(polar == TRUE) { p \<- p +
coord\_polar() } return(p) } monthplot \<- function(x,…) {
UseMethod(‘monthplot’, x) }

link \<- ‘C:/Users/jpayne05/Desktop/Buildwas\_15min\_Flow.csv’ Buildwas
\<- loadAllFlow(link) Buildwas\_Analysis \<- hydroAggregate(Buildwas,
rolling\_aggregations = c(1, 2, 3, 4, 5, 6, 24, 120), method = ‘max’)

```` 

```{r}
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = FALSE)
````

``` {r}
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = TRUE)
```
