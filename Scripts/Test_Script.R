# Test script
devtools::load_all()
library(data.table)
library(ggplot2)
library(beepr)
library(RcppRoll)

#link <-'O:/National Modelling and Forecasting/21_Strategic Delivery/Flood Forecast Modelling/03 Training/HydroER_package/Aggregations/Buildwas_15min_Flow.csv'

# Load Buildwas data in
link <- 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv'
Buildwas <- loadAllFlow(link)

# Aggregate Buildwas
Buildwas_Analysis <- hydroAggregate(Buildwas, rolling_aggregations = c(1,2,3,4,5,6,24, 120), method = 'max')
Buildwas_Volume <- hydroAggregate(Buildwas, rolling_aggregations = c(1,2,3,4,5,6,24, 120), method = 'sum')
# View(Buildwas_Volume$Rolling_Aggregations)
# View(Buildwas_Analysis$Rolling_Aggregations)
summary(Buildwas_Analysis)


# Get AMAX using different methods wrapped into one function
getAMAX(Buildwas_Analysis)
getAMAX(Buildwas)

# Plot QMED
QMEDPlot(Buildwas)
QMEDPlot(Buildwas_Analysis)

# Get L- moments
Ls(Buildwas)

monthplot(Buildwas_Analysis, name = 'Buildwas', polar = FALSE)
monthplot(Buildwas_Analysis, name = 'Buildwas', polar = TRUE)
singleSite(Buildwas_Analysis[[5]]$HydrologicalYear, Buildwas_Analysis[[5]]$HydroYear_Max, flows = c(300,500, 980))
beepr::beep(sound = 3, expr = NULL)
