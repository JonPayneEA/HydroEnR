window.rainAllDaily <- function(x, start = NULL, end = NULL){
  if(is.null(start)) {
    first <- 1
  } else {
    first <- which(x$DateTime == as.POSIXct(start, tz = 'GMT'))[1]
  }
  if(is.null(end)) {
    last <- length(x$DateTime)
  } else {
    last <- which(x$DateTime == as.POSIXct(end, tz = 'GMT'))[1]
  }
  dt <- x[first:last,]
  return(dt)
}

window.rainAllDaily <- function(x, start = NULL, end = NULL){
  if(is.null(start)) {
    first <- 1
  } else {
    first <- which(x$DateTime == as.Date(start))[1]
  }
  if(is.null(end)) {
    last <- length(x$DateTime)
  } else {
    last <- which(x$DateTime == as.Date(end))[1]
  }
  dt <- x[first:last,]
  return(dt)
}

window <- function(x, start, end){
  useMethod('window', x)
}

z <- cumsumNA.rainAll(window.rainALL(rain, start = '2005-06-12 23:00:00'))

daily_cumsum <- cumsumNA.rainAll(window.rainAllDaily(zzz, start = '2005-06-12'))



window(zzz, start = '2005-06-12')

class(zzz)
