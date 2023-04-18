library(data.table)
#data.table::update.dev.pkg()


#' @title loadWISKI
#'
#' @description Load data downloaded from WISKI, in the EA ascii format. Metadata
#' are preserved. Loaded data are in the form of a list, called either flowLoad,
#' rainLoad, or stageLoad. Data are in two parts MetaData and GaugeData.
#'
#' @param link Link to the specified file for import
#' @param skip Stet to zero, denotes the number off rows you wish to skip
#' @param meta_rows Set to 15, determines the amount of rows set as metadata
#' @param hydro_year Set to 'oct_us_gb' (USA and UK), but can also be 'sep_br'
#' (Brazil),'apr_cl' (Chille).
#' @param cols Set to NULL, but can be set as a vector of column names. Note
#' the first 2 must be 'DateTime' and 'Value'
#' @param ... Extra parameters to add to the fread command
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' #csvfile <- 'C:/Users/jpayne05/Downloads/Wellingborough.csv'
#' #a <- loadWISKI(csvfile, showProgress = TRUE, verbose = TRUE)
loadWISKI <- function(link,
                      skip = 0,
                      meta_rows = 15,
                      hydro_year = 'oct_us_gb',
                      cols = NULL,
                      ...
                      ){
  cat('Importing meta data\n')
  rdata<- fread(link,
                nrows = meta_rows,
                skip = skip,
                sep = '\t',
                header = FALSE,
                col.names = c('Parameter', 'Data'),
                na.strings = c('---', 'NA')
  )
  col_row <- fread(link,
               nrows = 1,
               skip = 15,
               sep = ',',
               header = FALSE,
               na.strings = c('---', 'NA')
  )
  if(length(col_row) == 5)
    col_names <- c('DateTime', 'Value', 'ValueState', 'Tags', 'Remarks')
  if(length(col_row) == 7)
    col_names <- c('DateTime', 'Value', 'ValueState', 'Runoff', 'RunoffQuality',
                   'Tags', 'Remarks')
  if(!is.null(cols)){
    col_names <- cols
    if(length(cols) != length()){
      stop('The number of column names does not match the column number of the data table')
    }
    if(cols[1] != 'DateTime'){
      cols[1] <- 'DateTime'
      message('The first column has to be called "DateTime", this has been corerced')
    }
    if(cols[1] != 'Value'){
      cols[1] <- 'Value'
      message('The first column has to be called "Value", this has been corerced')
    }
  }
  if(rdata[9,2]=='Precipitation'){
    cat('Importing precipitation data\n')
    dt <- fread(link,
                skip =  meta_rows+skip,
                sep= ',',
                header=TRUE,
                na.strings= c('---', 'NA'),
                col.names = col_names,
                fill = TRUE,
                ...
                )
    cat('Removing blank elements at top\n')
    first_data <- min(which(dt$Value != "NA")) # Locates the first non NA value in the Values field
    dt <- dt[first_data: length(dt$Value)]
    cat('Converting dates and times\n')
    dt$DateTime <- as.POSIXct(dt$DateTime, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    hydroData <- HydroYearDay(as.Date(dt$DateTime), hy_cal = hydro_year)
    dt <- data.table(dt, hydroData)
  }
  if(rdata[9,2]=='Flow'){
    cat('Importing precipitation data\n')
    dt <- fread(link,
                skip =  meta_rows+skip,
                sep= ',',
                header=TRUE,
                na.strings= c('---', 'NA'),
                col.names = col_names,
                fill = TRUE,
                ...
    )
    cat('Removing blank elements at top\n')
    first_data <- min(which(dt$Value != "NA")) # Locates the first non NA value in the Values field
    dt <- dt[first_data: length(dt$Value)]
    cat('Converting dates and times\n')
    dt$DateTime <- as.POSIXct(dt$DateTime, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    hydroData <- HydroYearDay(as.Date(dt$DateTime), hy_cal = hydro_year)
    dt <- data.table(dt, hydroData)
  }
  if(rdata[9,2]=='Stage'){
    cat('Importing precipitation data\n')
    dt <- fread(link,
                skip =  meta_rows+skip,
                sep= ',',
                header=TRUE,
                na.strings= c('---', 'NA'),
                col.names = col_names,
                fill = TRUE,
                ...
    )
    cat('Removing blank elements at top\n')
    first_data <- min(which(dt$Value != "NA")) # Locates the first non NA value in the Values field
    dt <- dt[first_data: length(dt$Value)]
    cat('Converting dates and times\n')
    dt$DateTime <- as.POSIXct(dt$DateTime, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    hydroData <- HydroYearDay(as.Date(dt$DateTime), hy_cal = hydro_year)
    dt <- data.table(dt, hydroData)
  }
  data <- list()
  data[['Metadata']] <- rdata
  data[['GaugeData']] <- dt
  if(rdata[9,2]=='Precipitation'){
    class(data) <- append(class(data), 'rainLoad')
  }
  if(rdata[9,2]=='Flow'){
    class(data) <- append(class(data), 'flowLoad')
  }
  if(rdata[9,2]=='Stage'){
    class(data) <- append(class(data), 'stageLoad')
  }
  return(data)
}
# cumSkipNA <- function(x, FUNC){
#   d <- deparse(substitute(FUNC))
#   funs <- c("max", "min", "prod", "sum")
#   stopifnot(is.vector(x), is.numeric(x), d %in% funs)
#   FUNC <- match.fun(paste0("cum", d))
#   x[!is.na(x)] <- FUNC(x[!is.na(x)])
#   x
# }

# csvfile <- 'C:/Users/jpayne05/Downloads/Wellingborough.csv'
# a <- loadWISKI(csvfile)
# summary(a$GaugeData)
#
# plot(a$GaugeData$Value, type = 'l')
#
# cumsumRG <- cumSkipNA(a$GaugeData$Value, sum)
# plot(cumsumRG, type = 'l')
#
# x <- LETTERS[1:20]
# y <- paste0("var", seq(1,20))
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, 0, 5)
#
# # Heatmap
# library(ggplot2)
# ggplot(data, aes(X, Y, fill= Z)) +
#   geom_tile()


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

# zz <- rainMonthAgg(a$GaugeData, 'sum')
# zdates <- data.table(do.call('rbind', strsplit(as.character(zz$Year_Month),' ',fixed=TRUE)))
# colnames(zdates) <- c('Year', 'Month')
# zz <- data.table(zz, zdates)
# zz$Monthly_Sum[zz$Monthly_Sum == 0] <- NA
# zz$Year <- as.numeric(zz$Year)
# zz$Month <- as.numeric(zz$Month)

# library(ggplot2)
#
# ggplot(zz, aes(Month, Year, fill= Monthly_Sum)) +
#   geom_tile(color = "white",
#             lwd = 0.1,
#             linetype = 1) +
#   coord_fixed() +
#   #scale_fill_gradient(low = "red", high = "blue") +
#   # scale_colour_brewer(option = "magma") +
#   #scale_colour_brewer(palette = "Accent") +
#   # geom_text(aes(label = round(Monthly_Sum,2)), color = "white", size = 4) +
#   # scale_fill_gradient2(low = "#54BCE7",
#   #                      mid = "#0177BA",
#   #                      high = "#002B54") +
#   scale_fill_viridis_c(na.value = "grey88",
#                        direction = -1) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   scale_x_discrete(limits = month.abb) +
#   ggtitle('Wellingborough Rain Gauge Data')


