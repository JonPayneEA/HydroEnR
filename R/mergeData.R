#' @title Merge WISKI datasets
#'
#' @description Merges multiple time series data sets into a single data.table.
#' Pads shorter time series with NAs.
#'
#' @param ... Multiple flowLoad, rainLoad, and /or stageLoad class data.
#'
#' @return
#' @export
#'
#' @examples
#' rain <- mergeData(corby, brigstock, yelden, oundle, wellingborough, kingscliffe)
mergeData <- function(...){
  # Compile list of data tables
  dtlst <- list(...)
  # Extract the name for each dt
  names <- sapply(substitute(...()), deparse)
  # return(dtlst[[1]]$Metadata)
  for(i in seq_along(dtlst)){
    if(dtlst[[i]]$Metadata[9,2] != 'Precipitation'){
      stop('mergeData function is currrently only suitable for raingauge data. ',
           paste(names[i], ' is of type ', dtlst[[i]]$Metadata[9,2], sep = ''))
    }
  }
  # Replace Value as dt name
  for(i in seq_along(dtlst)){
    setnames(dtlst[[i]]$GaugeData, 2, names[i])
  }
  # Merge initial 2 dt's
  full_dt <- merge(dtlst[[1]]$GaugeData[,1:2],
                   dtlst[[2]]$GaugeData[,1:2],
                   by = 'DateTime',
                   all = T)
  # If more than 2 dt's loop through merge
  if(length(dtlst)>2) {
    for(i in tail(seq_along(dtlst), -2)){
      full_dt <- merge(full_dt,
                       dtlst[[i]]$GaugeData[,1:2],
                       by = 'DateTime',
                       all = T)
    }
  }
  # Change back to Value
  # Fixes issue that Loaded data has column 2 ID changed
  for(i in seq_along(dtlst)){
    setnames(dtlst[[i]]$GaugeData, 2, 'Value')
  }
  class(full_dt) <- append(class(full_dt), 'rainAll')
  return(full_dt)
}
