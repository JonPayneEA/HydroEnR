library(jsonlite)
library(data.table)
library(ggplot2)

#' @title Load hydrometric data off the EAs API
#'
#' @param WISKI WISKI ID for gauge
#' @param Parameter User dfined; 'flow', 'level', 'rain'
#' @param Period User defined 900 == 15min, 86400 == daily
#' @param Type Data type set as 'instantaneous', some sites will have different agregators applied
#' @param datapoints Set to standard that pulls up to 100,000 rows. If 'all' will pull up to 2,000,000 rows. 'earliest' returns first datapoint
#' @param UAT Set to FALSE, if TRUE will pull data from UAT environment
#'
#' @import jsonlite
#'
#' @return
#' @export
#'
#' @examples
#' hydroAPI(WISKI = 'SX87U053')
#' hydroAPI(WISKI = 'SX87U053', UAT = TRUE)
#'
#' hydroAPI(WISKI = '188753', UAT = FALSE)
#' hydroAPI(WISKI = '188753', UAT = TRUE)
#' # Pull data
#' hydroAPI(WISKI = 'SX87U053',
#'          Parameter = 'flow',
#'          Period = 900,
#'          Type = 'instantaneous',
#'          datapoints = 'standard')
#'
#' hydroAPI(WISKI = 'SX87U053',
#'          Parameter = 'level',
#'          Period = 900,
#'          Type = 'instantaneous',
#'          datapoints = 'standard',
#'          UAT = TRUE)
#'
#' hydroAPI(WISKI = '188753',
#'          Parameter = 'rainfall',
#'          Period = 86400,
#'          Type = 'total',
#'          datapoints = 'standard',
#'          UAT = TRUE)
#'
hydroAPI <- function(WISKI = NULL, Parameter = NULL, Period = NULL, Type = 'instantaneous', datapoints = 'standard', UAT = FALSE){
  if(UAT == FALSE){
    baselink <- 'http://environment.data.gov.uk/hydrology/id/stations.json'
    if(!is.null(WISKI)&is.null(Parameter)&is.null(Period)){
      link <- paste0(baselink, '?wiskiID=', WISKI)
      data <- jsonlite::fromJSON(link)
      data_level <- jsonlite::fromJSON(as.character(data$items[1]))
      params <- data.table::data.table(Parameter = data_level$items$measures[[1]]$parameter,
                                       Period = data_level$items$measures[[1]]$period,
                                       Type = data_level$items$measures[[1]]$valueType)
      # data$Type <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = data$Type , replacement = '')
      return(params)
    }
    if(!is.null(WISKI)&!is.null(Parameter)&!is.null(Period)){
      link <- paste0(baselink, '?wiskiID=', WISKI)
      data <- jsonlite::fromJSON(link)
      data_level <- jsonlite::fromJSON(as.character(data$items[1]))
      params <- data.table::data.table(parameter = data_level$items$measures[[1]]$parameter,
                                       period = data_level$items$measures[[1]]$period,
                                       type = data_level$items$measures[[1]]$valueType,
                                       note = data_level$items$measures[[1]]$notation)
      datalink <- params[parameter == Parameter & period == Period & type == Type, note,]
      measure <- paste0('http://environment.data.gov.uk/hydrology/id/measures/',
                        datalink)
      if(datapoints == 'all')
        datalinkAppend <- paste0(measure, '/readings.json?_limit=2000000')
      if(datapoints == 'earliest')
        datalinkAppend <- paste0(measure, '/readings.json?earliest')
      if(datapoints == 'standard')
        datalinkAppend <- paste0(measure, '/readings.json')
      series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
      if(Period == 900){
        series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
      }
      if(Period == 86400){
        series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
      }
      return(series[,3:5])
    }
  }
  if(UAT == TRUE){
    baselink <- 'https://hydrology-uat.epimorphics.net/hydrology/id/stations.json'
    if(!is.null(WISKI)&is.null(Parameter)&is.null(Period)){
      link <- paste0(baselink, '?wiskiID=', WISKI)
      data <- data.table(jsonlite::fromJSON(link)[[2]]$measures[[1]])
      colnames(data) <- c('ID', 'Parameter', 'Period', 'Type')
      data$Type <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = data$Type , replacement = '')
      return(data[,2:4])
    }
    if(!is.null(WISKI)&!is.null(Parameter)&!is.null(Period)){
      link <- paste0(baselink, '?wiskiID=', WISKI)
      data <- data.table(jsonlite::fromJSON(link)[[2]]$measures[[1]])
      colnames(data) <- c('ID', 'parameter', 'period', 'valueType')
      data$valueType <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = data$valueType , replacement = '')
      datalink <- data[parameter == Parameter & period == Period & valueType == Type, ID,]
      datalinkupd <- summary(url(datalink))$description
      cut <- gsub(pattern = '(http[s]?://)?([^/s]+/)', x = datalinkupd , replacement = '')
      uat <- paste0('https://hydrology-uat.epimorphics.net/hydrology/id/',
                    cut) # During UAT we have to swap to the UAT portal
      # data_level <- jsonlite::fromJSON(as.character(datalink))
      if(datapoints == 'all')
        datalinkAppend <- paste0(uat, '/readings.json?_limit=2000000')
      if(datapoints == 'earliest')
        datalinkAppend <- paste0(uat, '/readings.json?earliest')
      if(datapoints == 'standard')
        datalinkAppend <- paste0(uat, '/readings.json')
      series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
      if(Period == 900){
        series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
      }
      if(Period == 86400){
        series$dateTime <- as.POSIXct(series$dateTime, format = '%Y-%m-%dT%H:%M', tz = 'GMT')
      }
      return(series[,3:6])
    }
  }
}

