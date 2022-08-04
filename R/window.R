#' @title Time (Series) Windows
#'
#' @description Window is a generic function which extracts the subset of the
#' object x observed between the times start and end. If a frequency is specified, the
#' series is then re-sampled at the new frequency.
#'
#' @description Currently this is only valid with merged rainfall data tables.
#'
#' @description Future updates will include application to other data sets
#'
#' @param x a data table or other input table
#' @param start Start date and time. If NULL it will default to the start of the
#' series.
#' @param end End date and time. If NULL it will default to the end of the
#' series
#'
#' @return
#' @export
#'
#' @examples
#' window(rain, start = '1988-06-12 23:30')
# window <- function(x, start, end){
#   UseMethod('window', x)
# }

#' @rdname window
#' @export
window.rainLoad <- function(x, start = NULL, end = NULL){
  cls <- tail(class(x), 1)
  if(is.null(start)) {
    first <- 1
  } else {
    first <- which(x$GaugeData$DateTime == as.POSIXct(start, tz = 'GMT'))[1]
  }
  if(is.null(end)) {
    last <- length(x$GaugeData$DateTime)
  } else {
    last <- which(x$GaugeData$DateTime == as.POSIXct(end, tz = 'GMT'))[1]
  }
  data <- list()
  # class(data) <- class(x)
  # return(x$Metadata)
  data[['Metadata']] <- x$Metadata
  data[['GaugeData']] <- x$GaugeData[first:last,]
  class(data) <- append(class(data), cls)
  return(data)
}

#' @rdname window
#' @export
window.flowLoad <- function(x, start = NULL, end = NULL){
  cls <- tail(class(x), 1)
  if(is.null(start)) {
    first <- 1
  } else {
    first <- which(x$GaugeData$DateTime == as.POSIXct(start, tz = 'GMT'))[1]
  }
  if(is.null(end)) {
    last <- length(x$GaugeData$DateTime)
  } else {
    last <- which(x$GaugeData$DateTime == as.POSIXct(end, tz = 'GMT'))[1]
  }
  data <- list()
  # class(data) <- class(x)
  # return(x$Metadata)
  data[['Metadata']] <- x$Metadata
  data[['GaugeData']] <- x$GaugeData[first:last,]
  class(data) <- append(class(data), cls)
  return(data)
}

#' @rdname window
#' @export
window.stageLoad <- function(x, start = NULL, end = NULL){
  cls <- tail(class(x), 1)
  if(is.null(start)) {
    first <- 1
  } else {
    first <- which(x$GaugeData$DateTime == as.POSIXct(start, tz = 'GMT'))[1]
  }
  if(is.null(end)) {
    last <- length(x$GaugeData$DateTime)
  } else {
    last <- which(x$GaugeData$DateTime == as.POSIXct(end, tz = 'GMT'))[1]
  }
  data <- list()
  # class(data) <- class(x)
  # return(x$Metadata)
  data[['Metadata']] <- x$Metadata
  data[['GaugeData']] <- x$GaugeData[first:last,]
  class(data) <- append(class(data), cls)
  return(data)
}

#' @rdname window
#' @export
window.rainAll <- function(x, start = NULL, end = NULL){
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

#' @rdname window
#' @export
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


