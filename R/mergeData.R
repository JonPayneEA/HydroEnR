mergeData <- function(...){
  # Compile list of data tables
  dtlst <- list(...)
  # Extract the name for each dt
  names <- sapply(substitute(...()), deparse)
  # Replace Value as dt name
  for(i in seq_along(dtlst)){
    colnames(dtlst[[i]]$GaugeData)[colnames(dtlst[[i]]$GaugeData) == 'Value'] <- names[i]
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
  class(full_dt) <- append(class(full_dt), 'rainAll')
  return(full_dt)
}

rain <- mergeData(corby, brigstock, yelden, oundle, wellingborough, kingscliffe)
