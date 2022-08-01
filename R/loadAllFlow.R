# loadAllFlow
# Imports data exported from WISKI using fread()
# First 16 rows are skipped
# The WISKI format for missing data '---' are converted to NAs
# Datatimes are converted to POSIXct format and split into sepereate datae, time and hour columns
# Caculates hydrological year and day
# Exports data as optimised data.table format

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
  hydroData <- HydroYearDay(dt$Date, hy_cal = hydro_year)

  # Merge 2 tables
  cat("Compilation complete\n")
  dt_1 <- data.table(dt, hydroData)
  class(dt_1) <- append(class(dt_1), "FlowLoad")

  return(dt_1)

}
